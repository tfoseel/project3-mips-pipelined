	.data
data1:	.word	1
data2:	.word	2
data3:	.word	3
data4:	.word	4
	.text
main:
	la	$4, data1	# 00
	lw	$5, 4($4)	# 04
	lw	$6, 8($4)	# 08
	lw	$7, 12($4)	# 0c
	lw	$4, 0($4)	# 10

	addu	$8, $4, $5	# 14
	addu	$9, $6, $7	# 18
	subu	$16, $9, $8	# 1c
	addu	$2, $16, $0	# 20
