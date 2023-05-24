	.data
array:	.word	3
	.word	123
	.word	4346
array2:	.word	0x11111111
	.text
main:
	addiu	$2, $0, 1024	//00
	addu	$3, $2, $2	//04
	or	$4, $3, $2	//08
	sll	$6, $5, 16	//0c
	addiu	$7, $6, 9999	//10
	subu	$8, $7, $2	//14
	nor	$9, $4, $3	//18
	ori	$10, $2, 255	//1c
	srl	$11, $6, 5	//20
	la	$4, array2	//24: lui $4, $4, 0x1000 / 28: ori $4, $4, 0x000c
	and	$13, $11, $5	//2c
	andi	$14, $4, 100	//30
	lui	$17, 100	//34
	addiu	$2, $0, 0xa	//38
