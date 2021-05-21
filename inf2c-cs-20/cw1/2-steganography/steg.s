#=========================================================================
# Steganography
#=========================================================================
# Retrive a secret message from a given text.
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

input_text_file_name:         .asciiz  "input_steg.txt"
newline:                      .asciiz  "\n"
        
#-------------------------------------------------------------------------
# Global variables in memory
#-------------------------------------------------------------------------
# 
input_text:                   .space 10001       # Maximum size of input_text_file + NULL
.align 4                                         # The next field will be aligned

# You can add your data here!
word: .space 10001

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

# opening file for reading

        li   $v0, 13                    # system call for open file
        la   $a0, input_text_file_name  # input_text file name
        li   $a1, 0                     # flag for reading
        li   $a2, 0                     # mode is ignored
        syscall                         # open a file
        
        move $t5, $v0                   # save the file descriptor 

        # reading from file just opened

        move $t0, $0                    # idx = 0

READ_LOOP:                              # do {
        li   $v0, 14                    # system call for reading from file
        move $a0, $t5                   # file descriptor
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
        move $a0, $t5                   # file descriptor to close
        syscall                         # fclose(input_text_file)


#------------------------------------------------------------------
# End of reading file block.
#------------------------------------------------------------------


# You can add your code here!
li   $t0, 0           # position of word in the line
li   $t1, 0           # target position of word in the line 
li   $t2, 0           # current word index
li   $t3, 0           # current file text index
li   $t4, 0           # whether the above line has a word
decrypt:
	beq   $t5, $0, end
	lb   $t5, input_text($t3)
	beq   $t5, 10, word_break
	beq   $t5, 32, word_break
	sb   $t5, word($t2)
	addi   $t2, $t2, 1
	addi   $t3, $t3, 1
	j decrypt
	
word_break:
	sb   $0, word($t2)
	beq   $t5, 10, line_break
	beq   $t0, $t1, print_word
	j next

line_break:
	blt   $t0, $t1, print_line 
	beq   $t0, $t1, print_word
	j next
	
print_line:
	li   $v0, 11
	li   $a0, 10
	syscall 
	j next
	
print_word:
	beq  $t4, $0, print_word_string
	li   $v0, 11
	li   $a0, 32
	syscall
	
print_word_string:
	li   $v0, 4
	la   $a0, word
	syscall
	j next
	
next:
	li   $t2, 0 
	addi   $t3, $t3, 1
	beq   $t5, 10, nextline
	addi   $t0, $t0, 1	
	j decrypt
	
nextline:
	sge   $t4, $t0, $t1 
	li   $t0, 0 
	addi   $t1, $t1, 1
	j decrypt 
	
end:
	li   $v0, 11
	la   $a0, 10
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
