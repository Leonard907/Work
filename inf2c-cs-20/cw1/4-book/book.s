#=========================================================================
# Book Cipher Decryption
#=========================================================================
# Decrypts a given encrypted text with a given book.
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

input_text_file_name:         .asciiz  "input_book_cipher.txt"
book_file_name:               .asciiz  "book.txt"
newline:                      .asciiz  "\n"
        
#-------------------------------------------------------------------------
# Global variables in memory
#-------------------------------------------------------------------------
# 
input_text:                   .space 10001       # Maximum size of input_text_file + NULL
.align 4                                         # The next field will be aligned
book:                         .space 10001       # Maximum size of book_file + NULL
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


# opening file for reading (book)

        li   $v0, 13                    # system call for open file
        la   $a0, book_file_name        # book file name
        li   $a1, 0                     # flag for reading
        li   $a2, 0                     # mode is ignored
        syscall                         # open a file
        
        move $s0, $v0                   # save the file descriptor 

        # reading from file just opened

        move $t0, $0                    # idx = 0

READ_LOOP1:                              # do {
        li   $v0, 14                    # system call for reading from file
        move $a0, $s0                   # file descriptor
                                        # book[idx] = c_input
        la   $a1, book($t0)              # address of buffer from which to read
        li   $a2,  1                    # read 1 char
        syscall                         # c_input = fgetc(book_file);
        blez $v0, END_LOOP1              # if(feof(book_file)) { break }
        lb   $t1, book($t0)          
        beq  $t1, $0,  END_LOOP1        # if(c_input == '\0')
        addi $t0, $t0, 1                # idx += 1
        j    READ_LOOP1
END_LOOP1:
        sb   $0,  book($t0)             # book[idx] = '\0'

        # Close the file 

        li   $v0, 16                    # system call for close file
        move $a0, $s0                   # file descriptor to close
        syscall                         # fclose(book_file)

#------------------------------------------------------------------
# End of reading file block.
#------------------------------------------------------------------

addi $sp, $sp, -4
sw   $ra, 0($sp)

# You can add your code here!
li   $t0, 0                     # index of text input
li   $t1, 0                     # line number or word number
li   $t2, 0                     # file text index
li   $t3, 0                     # word index
li   $t4, 0                     # word number in the row
li   $t5, 0                     # line number in the book
li   $t6, 0                     # target line number
li   $t7, 0                     # target word number
li   $t9, 0                     # whether line above has keyword
read_input:
	lb   $t8, input_text($t0) 
	addi   $t0, $t0, 1
	beq   $t8, $0, end
	beq   $t8, 32, place_line_number
	beq   $t8, 10, place_word_number
	addi   $t8, $t8, -48
	mul   $t2, $t2, 10
	add   $t2, $t2, $t8
	j read_input	
	
place_line_number:
	addi   $t2, $t2, -1
	move   $t6, $t2
	li   $t2, 0
	j read_input
	
place_word_number:
	addi   $t2, $t2, -1
	move   $t7, $t2
	li   $t2, 0
	beq   $t8, 10, find_word
	j read_input
	
find_word:
	lb   $t8, book($t2)
	beq   $t8, 32, word_break
	beq   $t8, 10, word_break
	sb   $t8, word($t3)
	addi   $t2, $t2, 1
	addi   $t3, $t3, 1
	j find_word 
	
word_break:
	sb   $0, word($t3)
	beq   $t6, $t5, check_word
	beq   $t8, 10, continue
pass:
	addi   $t4, $t4, 1
	li   $t3, 0
	addi   $t2, $t2, 1
	j find_word
	
check_word:
	beq   $t4, $t7, match	
	beq   $t8, 10, no_match
	j pass
	
no_match:
	li   $v0, 11 
	li   $a0, 10
	syscall 
	li   $t9, 0
	jal reset_book
	j read_input
continue:
	li   $t4, 0
	addi   $t5, $t5, 1
	addi   $t2, $t2, 1
	li   $t3, 0
	j find_word
	
match:
	beq   $t9, $0, print_word
	li   $v0, 11 
	li   $a0, 32
	syscall 
print_word:
	li   $v0, 4
	la   $a0, word	
	syscall
	li   $t9, 1
	jal reset_book
	j read_input
	
reset_book:
	li   $t2, 0
	li   $t3, 0
	li   $t4, 0
	li   $t5, 0 
	li   $t6, 0
	li   $t7, 0
	jr   $ra

end:
	li   $v0, 11
	la   $a0, 10
	syscall
	
lw   $ra, 0($sp)
addi $sp, $sp, 4

#------------------------------------------------------------------
# Exit, DO NOT MODIFY THIS BLOCK
#------------------------------------------------------------------
main_end:      
        li   $v0, 10          # exit()
        syscall

#----------------------------------------------------------------
# END OF CODE
#----------------------------------------------------------------
