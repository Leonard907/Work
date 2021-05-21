#=========================================================================
# XOR Cipher Encryption
#=========================================================================
# Encrypts a given text with a given key.
# 
# Inf2C Computer Systems
# 
# Dmitrii Ustiugov
# 9 Oct 2020
# 
#
#=========================================================================
# DATA SEGMENT
#=========================================================================
.data
#-------------------------------------------------------------------------
# Constant strings
#-------------------------------------------------------------------------

input_text_file_name:         .asciiz  "input_xor.txt"
key_file_name:                .asciiz  "key_xor.txt"
newline:                      .asciiz  "\n"
        
#-------------------------------------------------------------------------
# Global variables in memory
#-------------------------------------------------------------------------
# 
input_text:                   .space 10001       # Maximum size of input_text_file + NULL
.align 4                                         # The next field will be aligned
key:                          .space 33           # Maximum size of key_file + NULL
.align 4                                         # The next field will be aligned

# You can add your data here!
int_key:  .space 4

#=========================================================================
# TEXT SEGMENT  
#=========================================================================
.text

#-------------------------------------------------------------------------
# MAIN code block
#-------------------------------------------------------------------------

.globl main                     # Declare main label to be globally visible.
                                # Needed for correct operation with MARS
main:
#-------------------------------------------------------------------------
# Reading file block. DO NOT MODIFY THIS BLOCK
#-------------------------------------------------------------------------

# opening file for reading (text)

        li   $v0, 13                    # system call for open file
        la   $a0, input_text_file_name  # input_text file name
        li   $a1, 0                     # flag for reading
        li   $a2, 0                     # mode is ignored
        syscall                         # open a file
        
        move $s0, $v0                   # save the file descriptor 

        # reading from file just opened

        move $t0, $0                    # idx = 0

READ_LOOP:                              # do {
        li   $v0, 14                    # system call for reading from file
        move $a0, $s0                   # file descriptor
                                        # input_text[idx] = c_input
        la   $a1, input_text($t0)             # address of buffer from which to read
        li   $a2,  1                    # read 1 char
        syscall                         # c_input = fgetc(input_text_file);
        blez $v0, END_LOOP              # if(feof(input_text_file)) { break }
        lb   $t1, input_text($t0)          
        beq  $t1, $0,  END_LOOP        # if(c_input == '\0')
        addi $t0, $t0, 1                # idx += 1
        j    READ_LOOP
END_LOOP:
        sb   $0,  input_text($t0)       # input_text[idx] = '\0'

        # Close the file 

        li   $v0, 16                    # system call for close file
        move $a0, $s0                   # file descriptor to close
        syscall                         # fclose(input_text_file)


# opening file for reading (key)

        li   $v0, 13                    # system call for open file
        la   $a0, key_file_name         # key file name
        li   $a1, 0                     # flag for reading
        li   $a2, 0                     # mode is ignored
        syscall                         # open a file
        
        move $s0, $v0                   # save the file descriptor 

        # reading from file just opened

        move $t0, $0                    # idx = 0

READ_LOOP1:                              # do {
        li   $v0, 14                    # system call for reading from file
        move $a0, $s0                   # file descriptor
                                        # key[idx] = c_input
        la   $a1, key($t0)              # address of buffer from which to read
        li   $a2,  1                    # read 1 char
        syscall                         # c_input = fgetc(key_file);
        blez $v0, END_LOOP1              # if(feof(key_file)) { break }
        lb   $t1, key($t0)          
        addi $v0, $0, 10                # newline \n
        beq  $t1, $v0, END_LOOP1         # if(c_input == '\n')
        addi $t0, $t0, 1                # idx += 1
        j    READ_LOOP1
END_LOOP1:
        sb   $0,  key($t0)             # key[idx] = '\0'

        # Close the file 

        li   $v0, 16                    # system call for close file
        move $a0, $s0                   # file descriptor to close
        syscall                         # fclose(key_file)

#------------------------------------------------------------------
# End of reading file block.
#------------------------------------------------------------------


# You can add your code here!
li   $t0, 0           # key length in bytes
li   $t1, 0           # current byte value
li   $t2, 7         # current bit value (left shift)
li   $t3, 0           # key index
length:
	lb   $t4, key($t3)
	beq   $t4, $0, setup
	sub   $t4, $t4, 48
	sllv   $t4, $t4, $t2
	add   $t1, $t1, $t4
	addi   $t3, $t3, 1
	addi   $t2, $t2, -1
	bne   $t2, -1, length 
	sb   $t1, int_key($t0)
	addi   $t0, $t0, 1 
	li   $t1, 0
	li   $t2, 7
	j length

setup:
	li   $t1, 0        # current letter position
	li   $t2, 0        # file text position
encrypt:
	lb   $t3, input_text($t2)
	beq   $t3, $0, main_end
	beq   $t3, 32, pass
	beq   $t3, 10, pass
	lb   $t4, int_key($t1)
	xor   $t4, $t4, $t3 
	li   $v0, 11
	move   $a0, $t4 
	syscall
next:
	addi   $t1, $t1, 1
	addi   $t2, $t2, 1
	bne   $t1, $t0, encrypt
	li   $t1, 0
	j encrypt
	
pass:
	li   $v0, 11
	move   $a0, $t3 
	syscall
	j next
	
#------------------------------------------------------------------
# Exit, DO NOT MODIFY THIS BLOCK
#------------------------------------------------------------------
main_end:      
        li   $v0, 10          # exit()
        syscall

#----------------------------------------------------------------
# END OF CODE
#----------------------------------------------------------------
