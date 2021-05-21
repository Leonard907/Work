#=========================================================================
# XOR Cipher Cracking
#=========================================================================
# Finds the secret key for a given encrypted text with a given hint.
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

input_text_file_name:         .asciiz  "input_xor_crack.txt"
hint_file_name:                .asciiz  "hint.txt"
newline:                      .asciiz  "\n"
        
#-------------------------------------------------------------------------
# Global variables in memory
#-------------------------------------------------------------------------
# 
input_text:                   .space 10001       # Maximum size of input_text_file + NULL
.align 4                                         # The next field will be aligned
hint:                         .space 101         # Maximum size of key_file + NULL
.align 4                                         # The next field will be aligned
decrypt_msg: 		      .space 10001
.align 4 

# You can add your data here!

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


# opening file for reading (hint)

        li   $v0, 13                    # system call for open file
        la   $a0, hint_file_name        # hint file name
        li   $a1, 0                     # flag for reading
        li   $a2, 0                     # mode is ignored
        syscall                         # open a file
        
        move $s0, $v0                   # save the file descriptor 

        # reading from file just opened

        move $t0, $0                    # idx = 0

READ_LOOP1:                              # do {
        li   $v0, 14                    # system call for reading from file
        move $a0, $s0                   # file descriptor
                                        # hint[idx] = c_input
        la   $a1, hint($t0)             # address of buffer from which to read
        li   $a2,  1                    # read 1 char
        syscall                         # c_input = fgetc(key_file);
        blez $v0, END_LOOP1              # if(feof(key_file)) { break }
        lb   $t1, hint($t0)          
        addi $v0, $0, 10                # newline \n
        beq  $t1, $v0, END_LOOP1         # if(c_input == '\n')
        addi $t0, $t0, 1                # idx += 1
        j    READ_LOOP1
END_LOOP1:
        sb   $0,  hint($t0)             # hint[idx] = '\0'

        # Close the file 

        li   $v0, 16                    # system call for close file
        move $a0, $s0                   # file descriptor to close
        syscall                         # fclose(key_file)

#------------------------------------------------------------------
# End of reading file block.
#------------------------------------------------------------------
	

# You can add your code here!
li   $t0, 0
process_hint:
	lb   $t1, hint($t0)
	beq   $t1, $0, main_process
	beq   $t1, 32, replace
	addi   $t0, $t0, 1
	j process_hint
	
replace:
	li   $t1, 10
	sb   $t1, hint($t0)
	addi   $t0, $t0, 1
	j process_hint

main_process:
li   $t0, 0                 # the offset
li   $t1, 0                 # input text index
decrypt:
	beq   $t0, 256, no_key
	lb   $t2, input_text($t1)
	beq   $t2, 32, pass
	beq   $t2, 10, pass
	beq   $t2, 0, end
	xor   $t2, $t2, $t0
	sb   $t2, decrypt_msg($t1)
	addi   $t1, $t1, 1
	j decrypt
	
pass:
	li   $t2, 10
end:
	sb   $t2, decrypt_msg($t1)
	addi   $t1, $t1, 1
	beq   $t2, $0, setup
	j decrypt

setup:
#li    $v0, 4
#la    $a0, decrypt_msg
#syscall
#li    $v0, 11
#li    $a0, 10
#syscall
li    $t1, 0                # start index of text
li    $t2, 0                # index of hint
li    $t3, 0                # index of text
	
check_word:
	lb   $t6, decrypt_msg($t1)
	beq   $t6, $0, next_key
	lb   $t4, hint($t2)
	lb   $t5, decrypt_msg($t3)
	beq   $t4, $0, match
	beq   $t4, $t5, continue
no_match:
	addi   $t1, $t1, 1
	move   $t3, $t1
	li   $t2, 0
	j check_word
	
match:
	j output
	
continue:
	addi   $t2, $t2, 1
	addi   $t3, $t3, 1
	j check_word
	
next_key:
	addi   $t0, $t0, 1
	li   $t1, 0
	j decrypt
	
no_key:
	li   $v0, 11
	li   $a0, 45
	syscall
	li   $v0, 1
	li   $a0, 1
	syscall
	j main_end
	
output:
	li   $t1, 128               # counter
print_key:
	beq   $t1, $0, print_new_line
	bge   $t0, $t1, print_one
	srl   $t1, $t1, 1
	li   $v0, 1
	li   $a0, 0
	syscall
	j print_key
	
print_one:
	sub   $t0, $t0, $t1
	srl   $t1, $t1, 1
	li   $v0, 1
	li   $a0, 1
	syscall
	j print_key
	
print_new_line:
	li   $v0, 11
	li   $a0, 10
	syscall	
	
#------------------------------------------------------------------
# Exit, DO NOT MODIFY THIS BLOCK
#------------------------------------------------------------------
main_end:      
        li   $v0, 10          # exit()
        syscall

#----------------------------------------------------------------
# END OF CODE
#----------------------------------------------------------------
