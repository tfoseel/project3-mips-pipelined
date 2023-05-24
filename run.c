/***************************************************************/
/*                                                             */
/*   MIPS-32 Instruction Level Simulator                       */
/*                                                             */
/*   CS311 KAIST                                               */
/*   run.c                                                     */
/*                                                             */
/***************************************************************/

// Uncomment below line when debugging
// #define DEBUG

#include <stdio.h>

#include "util.h"
#include "run.h"

/***************************************************************/
/*                                                             */
/* Procedure: get_inst_info                                    */
/*                                                             */
/* Purpose: Read insturction information                       */
/*                                                             */
/***************************************************************/
instruction *get_inst_info(uint32_t pc)
{
    return &INST_INFO[(pc - MEM_TEXT_START) >> 2];
}

/* Helper functions processing each stage of pipeline. */

#define CS (CURRENT_STATE)

int WB_CNT = 0;
int stall_bit = FALSE;
int branch_flush_bit = FALSE;
int jump_flush_bit = FALSE;

int is_final()
{
    for (int i = 0; i < PIPE_STAGE - 1; i++)
    {
        if (CS.PIPE[i] != 0)
            return FALSE;
    }
    return TRUE;
}

int regWrite(short opcode, short funct)
{
    switch (opcode)
    {
    // I format
    case 0x9:  // ADDIU
    case 0xc:  // ANDI
    case 0xf:  // LUI
    case 0xd:  // ORI
    case 0xb:  // SLTIU
    case 0x23: // LW
        return TRUE;
    case 0x2b: // SW
    case 0x4:  // BEQ
    case 0x5:  // BNE
        return FALSE;
    // R format
    case 0x0:
        switch (funct)
        {
        case 0x21: // ADDU
        case 0x24: // AND
        case 0x27: // NOR
        case 0x25: // OR
        case 0x2b: // SLTU
        case 0x00: // SLL
        case 0x02: // SRL
        case 0x23: // SUBU
            return TRUE;
        case 0x08: // JR
            return FALSE;
        }
    // J format
    case 0x2: // J
        return FALSE;
    case 0x3: // JAL
        return TRUE;
    default:
        printf("Not available instruction\n");
        assert(0);
    }
}

int memRead(short opcode)
{
    return opcode == 0x23;
}

int memWrite(short opcode)
{
    return opcode == 0x2b;
}

int is_load_use()
{
    if (memRead(CS.EX_MEM_OPCODE) && ((CS.EX_MEM_DEST == CS.ID_EX_REGNUM1) || (CS.EX_MEM_DEST == CS.ID_EX_REGNUM2)))
    {
#ifdef DEBUG
        printf("Stall needed between 0x%x, 0x%x\n", CS.IF_ID_NPC, CS.ID_EX_NPC);
#endif
        return TRUE;
    }
    else
        return FALSE;
}

void try_forward()
{
#ifdef DEBUG
    printf("EX/MEM_FR: %d, EX/MEM_FV: %d(0x%x), MEM/WB_FR: %d, MEM/WB_FV: %d(0x%x)\n",
           CS.EX_MEM_FORWARD_REG, CS.EX_MEM_FORWARD_VALUE, CS.EX_MEM_FORWARD_VALUE,
           CS.MEM_WB_FORWARD_REG, CS.MEM_WB_FORWARD_VALUE, CS.MEM_WB_FORWARD_VALUE);
#endif
    int EX_MEM_EX_REG1_FORWARD = regWrite(CS.EX_MEM_OPCODE, CS.EX_MEM_FUNCT) &&
                                 (CS.EX_MEM_FORWARD_REG != 0) && (CS.EX_MEM_FORWARD_REG == CS.ID_EX_REGNUM1);
    int EX_MEM_EX_REG2_FORWARD = regWrite(CS.EX_MEM_OPCODE, CS.EX_MEM_FUNCT) &&
                                 (CS.EX_MEM_FORWARD_REG != 0) && (CS.EX_MEM_FORWARD_REG == CS.ID_EX_REGNUM2);
    int MEM_WB_EX_REG1_FORWARD = regWrite(CS.MEM_WB_OPCODE, CS.MEM_WB_FUNCT) &&
                                 (CS.MEM_WB_FORWARD_REG != 0) &&
                                 (!EX_MEM_EX_REG1_FORWARD) &
                                     (CS.MEM_WB_FORWARD_REG == CS.ID_EX_REGNUM1);
    int MEM_WB_EX_REG2_FORWARD = regWrite(CS.MEM_WB_OPCODE, CS.MEM_WB_FUNCT) &&
                                 (CS.MEM_WB_FORWARD_REG != 0) &&
                                 (!EX_MEM_EX_REG2_FORWARD) &&
                                 (CS.MEM_WB_FORWARD_REG == CS.ID_EX_REGNUM2);
    int MEM_WB_MEM_FORWARD = memRead(CS.MEM_WB_OPCODE) && memWrite(CS.EX_MEM_OPCODE) &&
                             (CS.MEM_WB_LOAD_STORE_FORWARD_REG != 0) &&
                             (CS.MEM_WB_LOAD_STORE_FORWARD_REG == CS.EX_MEM_REGNUM2);
    // EX/MEM to EX forwarding
    if (EX_MEM_EX_REG1_FORWARD)
    {
#ifdef DEBUG
        printf("EX Forward! %d %d value : %d\n", CS.EX_MEM_FORWARD_REG, CS.ID_EX_REGNUM1, CS.EX_MEM_FORWARD_VALUE);
#endif
        CS.ID_EX_REG1 = CS.EX_MEM_FORWARD_VALUE;
    }
    if (EX_MEM_EX_REG2_FORWARD)
    {
#ifdef DEBUG
        printf("EX Forward! %d %d value : %d\n", CS.EX_MEM_FORWARD_REG, CS.ID_EX_REGNUM2, CS.EX_MEM_FORWARD_VALUE);
#endif
        CS.ID_EX_REG2 = CS.EX_MEM_FORWARD_VALUE;
    }

    // MEM/WB to EX forwarding
    if (MEM_WB_EX_REG1_FORWARD)
    {
#ifdef DEBUG
        printf("MEM Forward! %d %d value : %d\n", CS.MEM_WB_FORWARD_REG, CS.ID_EX_REGNUM1, CS.MEM_WB_FORWARD_VALUE);
#endif
        CS.ID_EX_REG1 = CS.MEM_WB_FORWARD_VALUE;
    }
    if (MEM_WB_EX_REG2_FORWARD)
    {
#ifdef DEBUG
        printf("MEM Forward! %d %d value : %d\n", CS.MEM_WB_FORWARD_REG, CS.ID_EX_REGNUM2, CS.MEM_WB_FORWARD_VALUE);
#endif
        CS.ID_EX_REG2 = CS.MEM_WB_FORWARD_VALUE;
    }

    // MEM/WB to MEM forwarding
    if (MEM_WB_MEM_FORWARD)
    {
#ifdef DEBUG
        printf("MEM(LOAD&STORE) Forward! %d %d value : %d\n",
               CS.MEM_WB_LOAD_STORE_FORWARD_REG, CS.EX_MEM_REGNUM2, CS.MEM_WB_LOAD_STORE_FORWARD_VALUE);
#endif
        CS.EX_MEM_W_VALUE = CS.MEM_WB_LOAD_STORE_FORWARD_VALUE;
    }
}

void clear_MEM_WB()
{
    CS.MEM_WB_NPC = 0;
    CS.MEM_WB_ALU_OUT = 0;
    CS.MEM_WB_MEM_OUT = 0;
    CS.MEM_WB_BR_TAKE = 0;
    CS.MEM_WB_OPCODE = 0;
    CS.MEM_WB_FUNCT = 0;
    CS.MEM_WB_DEST = 0;
}

void clear_EX_MEM()
{
    CS.EX_MEM_NPC = 0;
    CS.EX_MEM_ALU_OUT = 0;
    CS.EX_MEM_W_VALUE = 0;
    CS.EX_MEM_BR_TARGET = 0;
    CS.EX_MEM_BR_TAKE = 0;
    CS.EX_MEM_OPCODE = 0;
    CS.EX_MEM_FUNCT = 0;
    CS.EX_MEM_REGNUM2 = 0;
    CS.EX_MEM_DEST = 0;
}

void clear_ID_EX()
{
    CS.ID_EX_NPC = 0;
    CS.ID_EX_REGNUM1 = 0;
    CS.ID_EX_REGNUM2 = 0;
    CS.ID_EX_REG1 = 0;
    CS.ID_EX_REG2 = 0;
    CS.ID_EX_JUMP_TARGET = 0;
    CS.ID_EX_IMM = 0;
    CS.ID_EX_OPCODE = 0;
    CS.ID_EX_FUNCT = 0;
    CS.ID_EX_SHAMT = 0;
    CS.ID_EX_DEST = 0;
}

void clear_IF_ID()
{
    CS.IF_ID_NPC = 0;
    CS.IF_ID_INST = 0;
    CS.IF_ID_REGNUM1 = 0;
    CS.IF_ID_REGNUM2 = 0;
}

void branch_flush()
{
    // Set flush bit
    branch_flush_bit = TRUE;

    // Prevent processing of IF, ID, EX stage
    CS.PIPE[IF_STAGE] = 0;
    CS.PIPE[ID_STAGE] = 0;
    CS.PIPE[EX_STAGE] = 0;

    // Clear previous latch
    clear_ID_EX();
    clear_IF_ID();
}

void jump_flush()
{
    // Set flush bit
    jump_flush_bit = TRUE;

    // Prevent processing of IF
    CS.PIPE[IF_STAGE] = 0;
}

void process_WB()
{
    // Move PC through pipeline
    CS.PIPE[WB_STAGE] = CS.MEM_WB_NPC;
    CS.MEM_WB_NPC = 0;
    // If PC is invalid, then do nothing
    if (CS.PIPE[WB_STAGE] == 0)
        return;
    // Do WB stage
    if (CS.MEM_WB_OPCODE == 0x23)
    {
        CS.REGS[CS.MEM_WB_DEST] = CS.MEM_WB_MEM_OUT;
#ifdef DEBUG
        printf("WB Register : %d, value : %d\n", CS.MEM_WB_DEST, CS.REGS[CS.MEM_WB_DEST]);
#endif
    }
    else if (regWrite(CS.MEM_WB_OPCODE, CS.MEM_WB_FUNCT))
    {
        CS.REGS[CS.MEM_WB_DEST] = CS.MEM_WB_ALU_OUT;
#ifdef DEBUG
        printf("WB Register : %d, value : %d\n", CS.MEM_WB_DEST, CS.REGS[CS.MEM_WB_DEST]);
#endif
    }
    WB_CNT++;
}

void process_MEM()
{
    clear_MEM_WB();
    // Move PC through pipeline
    CS.PIPE[MEM_STAGE] = CS.EX_MEM_NPC;
    CS.EX_MEM_NPC = 0;
    CS.MEM_WB_NPC = CS.PIPE[MEM_STAGE];
    // If PC is invalid, then do nothing
    if (CS.PIPE[MEM_STAGE] == 0)
        return;
    // Do MEM stage
    CS.MEM_WB_FUNCT = CS.MEM_WB_FUNCT;
    CS.MEM_WB_OPCODE = CS.EX_MEM_OPCODE;
    CS.MEM_WB_ALU_OUT = CS.EX_MEM_ALU_OUT;
    CS.MEM_WB_DEST = CS.EX_MEM_DEST;
    CS.MEM_WB_FORWARD_REG = CS.MEM_WB_DEST;
    CS.MEM_WB_LOAD_STORE_FORWARD_REG = CS.MEM_WB_DEST;
    CS.MEM_WB_FORWARD_VALUE = CS.EX_MEM_FORWARD_VALUE;
    if (CS.EX_MEM_BR_TAKE)
    {
#ifdef DEBUG
        printf("Branch taken!\n");
#endif
        CS.BRANCH_PC = CS.EX_MEM_BR_TARGET;
        branch_flush();
    }
    if (CS.EX_MEM_OPCODE == 0x23) // LW
    {
        CS.MEM_WB_MEM_OUT = mem_read_32(CS.EX_MEM_ALU_OUT);
        CS.MEM_WB_FORWARD_VALUE = CS.MEM_WB_MEM_OUT;
        CS.MEM_WB_LOAD_STORE_FORWARD_VALUE = CS.MEM_WB_MEM_OUT;
    }
    else if (CS.EX_MEM_OPCODE == 0x2b) // SW
    {
        mem_write_32(CS.EX_MEM_ALU_OUT, CS.EX_MEM_W_VALUE);
    }
}

void process_EX()
{
    if (branch_flush_bit)
    {
        return;
    }
    clear_EX_MEM();

    // Move PC through pipeline
    CS.PIPE[EX_STAGE] = CS.ID_EX_NPC;
    CS.EX_MEM_NPC = CS.PIPE[EX_STAGE];

    // If PC is invalid, then do nothing
    if (CS.PIPE[EX_STAGE] == 0)
        return;

    // Do EX stage
    {
        CS.EX_MEM_OPCODE = CS.ID_EX_OPCODE;
        CS.EX_MEM_FUNCT = CS.ID_EX_FUNCT;
        CS.EX_MEM_REGNUM2 = CS.ID_EX_REGNUM2;
        CS.EX_MEM_DEST = CS.ID_EX_DEST;

        int32_t signExtImm = 0;
        uint32_t zeroExtImm = 0;
        int32_t branchAddr = 0;

#ifdef DEBUG
        printf("EX: DEST: %d, REG1: %d, REG2: %d, IMM: %d\n", CS.ID_EX_DEST, CS.ID_EX_REG1, CS.ID_EX_REG2, CS.ID_EX_IMM);
#endif

        switch (CS.ID_EX_OPCODE)
        {
        // I format
        case 0x9: // ADDIU
            signExtImm =
                (CS.ID_EX_IMM & 0x00008000) == 0 ? CS.ID_EX_IMM : (CS.ID_EX_IMM | 0xFFFF0000);
            CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 + signExtImm;
            CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
            CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
            break;
        case 0xc: // ANDI
            zeroExtImm = CS.ID_EX_IMM;
            CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 & zeroExtImm;
            CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
            CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
            break;
        case 0xf: // LUI
            CS.EX_MEM_ALU_OUT = CS.ID_EX_IMM << 16;
            CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
            CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
            break;
        case 0xd: // ORI
            CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 | CS.ID_EX_IMM;
            CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
            CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
            break;
        case 0xb: // SLTIU
            CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 < CS.ID_EX_IMM ? 1 : 0;
            CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
            CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
            break;
        case 0x23: // LW
            signExtImm =
                (CS.ID_EX_IMM & 0x00008000) == 0 ? CS.ID_EX_IMM : (CS.ID_EX_IMM | 0xFFFF0000);
            CS.EX_MEM_FORWARD_REG = 0;
            CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 + signExtImm;
            break;
        case 0x2b: // SW
            signExtImm =
                (CS.ID_EX_IMM & 0x00008000) == 0 ? CS.ID_EX_IMM : (CS.ID_EX_IMM | 0xFFFF0000);
            CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 + signExtImm;
            CS.EX_MEM_W_VALUE = CS.ID_EX_REG2;
            break;
        case 0x4: // BEQ
            branchAddr =
                (CS.ID_EX_IMM & 0x00008000) == 0 ? CS.ID_EX_IMM << 2
                                                 : ((CS.ID_EX_IMM << 2) | 0xFFFC0000);
            CS.EX_MEM_BR_TAKE = CS.ID_EX_REG1 == CS.ID_EX_REG2;
            CS.EX_MEM_BR_TARGET = CS.PIPE[EX_STAGE] + 4 + branchAddr;
#ifdef DEBUG
            printf("Branch target : 0x%x, pc : 0x%x, addr : %d\n", CS.EX_MEM_BR_TARGET, CS.PIPE[EX_STAGE], branchAddr);
#endif
            break;
        case 0x5: // BNE
            branchAddr =
                (CS.ID_EX_IMM & 0x00008000) == 0 ? CS.ID_EX_IMM << 2
                                                 : ((CS.ID_EX_IMM << 2) | 0xFFFC0000);
            CS.EX_MEM_BR_TAKE = CS.ID_EX_REG1 != CS.ID_EX_REG2;
            CS.EX_MEM_BR_TARGET = CS.PIPE[EX_STAGE] + 4 + branchAddr;
#ifdef DEBUG
            printf("Branch target : 0x%x, pc : 0x%x, addr : %d\n", CS.EX_MEM_BR_TARGET, CS.PIPE[EX_STAGE], branchAddr);
#endif
            break;
        // R format
        case 0x0:
            switch (CS.ID_EX_FUNCT)
            {
            case 0x21: // ADDU
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 + CS.ID_EX_REG2;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x24: // AND
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 & CS.ID_EX_REG2;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x27: // NOR
                CS.EX_MEM_ALU_OUT = ~(CS.ID_EX_REG1 | CS.ID_EX_REG2);
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x25: // OR
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 | CS.ID_EX_REG2;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x2b: // SLTU
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 < CS.ID_EX_REG2 ? 1 : 0;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x00: // SLL
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG2 << CS.ID_EX_SHAMT;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x02: // SRL
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG2 >> CS.ID_EX_SHAMT;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x23: // SUBU
                CS.EX_MEM_ALU_OUT = CS.ID_EX_REG1 - CS.ID_EX_REG2;
                CS.EX_MEM_FORWARD_REG = CS.EX_MEM_DEST;
                CS.EX_MEM_FORWARD_VALUE = CS.EX_MEM_ALU_OUT;
                break;
            case 0x08: // JR
                // ???
                break;
            }
            break;
        // J format
        case 0x2: // J
            // ???
            break;
        case 0x3: // JAL
            // ???
            break;
        default:
            printf("Error in EX stage\n");
            assert(0);
        }
    }
}

void process_ID()
{
    if (branch_flush_bit)
    {
        return;
    }
    if (stall_bit)
    {
#ifdef DEBUG
        printf("ID stage is stalled\n");
#endif
        CS.PIPE[EX_STAGE] = 0;
        clear_EX_MEM();
        return;
    }
    clear_ID_EX();
    // Move PC through pipeline
    CS.PIPE[ID_STAGE] = CS.IF_ID_NPC;
    CS.IF_ID_NPC = 0;
    CS.ID_EX_NPC = CS.PIPE[ID_STAGE];
    // If PC is invalid, then do nothing
    if (CS.PIPE[ID_STAGE] == 0)
        return;

    // Do ID stage
    {
        instruction instr = *get_inst_info(CS.PIPE[ID_STAGE]);
        switch (instr.opcode)
        {
        // I format
        case 0x9:  // ADDIU
        case 0xc:  // ANDI
        case 0xf:  // LUI
        case 0xd:  // ORI
        case 0xb:  // SLTIU
        case 0x23: // LW
            CS.ID_EX_DEST = instr.r_t.r_i.rt;
            CS.ID_EX_IMM = instr.r_t.r_i.r_i.imm;
            break;
        case 0x2b: // SW
            CS.ID_EX_REG1 = instr.r_t.r_i.rs;
            CS.ID_EX_REG2 = instr.r_t.r_i.rt;
            CS.ID_EX_IMM = instr.r_t.r_i.r_i.imm;
            break;
        case 0x4: // BEQ
        case 0x5: // BNE
            CS.ID_EX_IMM = instr.r_t.r_i.r_i.imm;
            break;
        // R format
        case 0x0:
            switch (instr.func_code)
            {
            case 0x21: // ADDU
            case 0x24: // AND
            case 0x27: // NOR
            case 0x25: // OR
            case 0x2b: // SLTU
            case 0x00: // SLL
            case 0x02: // SRL
            case 0x23: // SUBU
                CS.ID_EX_DEST = instr.r_t.r_i.r_i.r.rd;
                CS.ID_EX_SHAMT = instr.r_t.r_i.r_i.r.shamt;
                CS.ID_EX_FUNCT = instr.func_code;
                break;
            case 0x08: // JR
                jump_flush();
                CS.JUMP_PC = CS.REGS[instr.r_t.r_i.rs];
#ifdef DEBUG
                printf("JR rs: %d\n", instr.r_t.r_i.rs);
                printf("JR PC : 0x%x\n", CS.JUMP_PC);
#endif
                break;
            }
            break;
        // J format
        case 0x2: // J
            jump_flush();
            CS.JUMP_PC = ((CS.IF_ID_NPC + 4) & 0xF0000000) + (instr.r_t.target << 2);
#ifdef DEBUG
            printf("J PC : 0x%x\n", CS.JUMP_PC);
#endif
            break;
        case 0x3: // JAL
            jump_flush();
            CS.REGS[31] = CS.PIPE[ID_STAGE] + 4;
            CS.JUMP_PC = (CS.PIPE[ID_STAGE] & 0xF0000000) + (instr.r_t.target << 2);
#ifdef DEBUG
            printf("JAL PC : 0x%x\n", CS.JUMP_PC);
#endif
            break;
        default:
            printf("Not available instruction\n");
            assert(0);
        }
        // Save to pipeline latch
        CS.ID_EX_OPCODE = instr.opcode;
        CS.ID_EX_REGNUM1 = instr.r_t.r_i.rs;
        CS.ID_EX_REGNUM2 = instr.r_t.r_i.rt;
        CS.ID_EX_REG1 = CS.REGS[instr.r_t.r_i.rs];
        CS.ID_EX_REG2 = CS.REGS[instr.r_t.r_i.rt];
    }
}

void process_IF()
{
    if (stall_bit)
    {
#ifdef DEBUG
        printf("IF stage is stalled\n");
#endif
        return;
    }
    if (branch_flush_bit)
    {
        CS.PC = CS.BRANCH_PC;
        CS.BRANCH_PC = 0;
        return;
    }
    clear_IF_ID();
    // Move PC through pipeline
    CS.PIPE[IF_STAGE] = (FETCH_BIT == FALSE) ? 0 : CS.PC;
    CS.IF_ID_NPC = CS.PIPE[IF_STAGE];
    // If PC is invalid, then do nothing
    if (CS.PIPE[IF_STAGE] == 0)
        return;
    // Do IF stage
    if (jump_flush_bit)
    {
        CS.IF_ID_NPC = 0;
        CS.PC = CS.JUMP_PC;
        CS.JUMP_PC = 0;
    }
    else
    {
        CS.IF_ID_NPC = CS.PC;
        CS.IF_ID_REGNUM1 = get_inst_info(CS.PC)->r_t.r_i.rs;
        CS.IF_ID_REGNUM2 = get_inst_info(CS.PC)->r_t.r_i.rt;
        CS.PC += 4; // Predict not taken
    }
}

/***************************************************************/
/*                                                             */
/* Procedure: process_instruction                              */
/*                                                             */
/* Purpose: Process one instrction                             */
/*                                                             */
/***************************************************************/
void process_instruction()
{
#ifdef DEBUG
    char c;
    printf("Proceed? (y/n)\n");
    int unused __attribute__((unused));
    unused = scanf("%s", &c);
    if (c != 'y')
    {
        printf("Halted\n");
        exit(0);
    }
#endif
    try_forward();
    process_WB();
    process_MEM();
    process_EX();
    process_ID();
    process_IF();
    stall_bit = is_load_use();
    branch_flush_bit = FALSE;
    jump_flush_bit = FALSE;
    // If EOF, do not fetch anymore.
    if (CS.PC >= MEM_TEXT_START + (NUM_INST << 2))
        FETCH_BIT = FALSE;
    // If final instruction, do not call this function anymore.
    if ((FETCH_BIT == FALSE && is_final()) || WB_CNT >= MAX_INSTRUCTION_NUM)
        RUN_BIT = FALSE;
}
