	.data
data1:	.word	100
data2:	.word	200
data3:	.word	0x12345678
	.word	0x12341
	.text
main:
	lui	$3, 0x1000	//00
	lw	$5, 0($3)	//04
	lw	$8, 4($3)	//08
	lw	$9, 8($3)	//0c
	lw	$10, 12($3)	//10
	addiu	$5, $5, 24	//14
	addiu	$6, $0, 124	//18
	addu	$7, $5, $6	//1c
	sw	$5, 16($3)	//20
	sw	$6, 20($3)	//24
	sw	$7, 24($3)	//28
	addiu	$3, $3, 12	//2c
	lw	$12, -4($3)	//30
	lw	$13, -8($3)	//34
	lw	$14, -12($3)	//38
