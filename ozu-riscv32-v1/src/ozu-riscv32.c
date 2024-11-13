#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "ozu-riscv32.h"

/***************************************************************/
/* Print out a list of commands available                      */
/***************************************************************/
void help() {        
	printf("------------------------------------------------------------------\n\n");
	printf("\t**********OZU-RV32 Disassembler and Simulator Help MENU**********\n\n");
	printf("sim\t-- simulate program to completion \n");
	printf("run <n>\t-- simulate program for <n> instructions\n");
	printf("rdump\t-- dump register values\n");
	printf("reset\t-- clears all registers/memory and re-loads the program\n");
	printf("input <reg> <val>\t-- set GPR <reg> to <val>\n");
	printf("mdump <start> <stop>\t-- dump memory from <start> to <stop> address\n");
	printf("print\t-- print the program loaded into memory\n");
	printf("?\t-- display help menu\n");
	printf("quit\t-- exit the simulator\n\n");
	printf("------------------------------------------------------------------\n\n");
}

/***************************************************************/
/* Read a 32-bit word from memory                              */
/***************************************************************/
uint32_t mem_read_32(uint32_t address)
{
	int i;
	for (i = 0; i < NUM_MEM_REGION; i++) {
		if ( (address >= MEM_REGIONS[i].begin) &&  ( address <= MEM_REGIONS[i].end) ) {
			uint32_t offset = address - MEM_REGIONS[i].begin;
			return (MEM_REGIONS[i].mem[offset+3] << 24) |
					(MEM_REGIONS[i].mem[offset+2] << 16) |
					(MEM_REGIONS[i].mem[offset+1] <<  8) |
					(MEM_REGIONS[i].mem[offset+0] <<  0);
		}
	}
	return 0;
}

/***************************************************************/
/* Write a 32-bit word to memory                               */
/***************************************************************/
void mem_write_32(uint32_t address, uint32_t value)
{
	int i;
	uint32_t offset;
	for (i = 0; i < NUM_MEM_REGION; i++) {
		if ( (address >= MEM_REGIONS[i].begin) && (address <= MEM_REGIONS[i].end) ) {
			offset = address - MEM_REGIONS[i].begin;

			MEM_REGIONS[i].mem[offset+3] = (value >> 24) & 0xFF;
			MEM_REGIONS[i].mem[offset+2] = (value >> 16) & 0xFF;
			MEM_REGIONS[i].mem[offset+1] = (value >>  8) & 0xFF;
			MEM_REGIONS[i].mem[offset+0] = (value >>  0) & 0xFF;
		}
	}
}

/***************************************************************/
/* Execute one cycle                                           */
/***************************************************************/
void cycle() {                                                
	handle_instruction();
	CURRENT_STATE = NEXT_STATE;
	INSTRUCTION_COUNT++;
}

/***************************************************************/
/* Simulate RISC-V for n cycles                                */
/***************************************************************/
void run(int num_cycles) {                                      
	
	if (RUN_FLAG == FALSE) {
		printf("Simulation Stopped\n\n");
		return;
	}

	printf("Running simulator for %d cycles...\n\n", num_cycles);
	int i;
	for (i = 0; i < num_cycles; i++) {
		if (RUN_FLAG == FALSE) {
			printf("Simulation Stopped.\n\n");
			break;
		}
		cycle();
	}
}

/***************************************************************/
/* simulate to completion                                      */
/***************************************************************/
void runAll() {                                                     
	if (RUN_FLAG == FALSE) {
		printf("Simulation Stopped.\n\n");
		return;
	}

	printf("Simulation Started...\n\n");
	while (RUN_FLAG){
		cycle();
	}
	printf("Simulation Finished.\n\n");
}

/**************************************************************************************/ 
/* Dump region of memory to the terminal (make sure provided address is word aligned) */
/**************************************************************************************/
void mdump(uint32_t start, uint32_t stop) {          
	uint32_t address;

	printf("-------------------------------------------------------------\n");
	printf("Memory content [0x%08x..0x%08x] :\n", start, stop);
	printf("-------------------------------------------------------------\n");
	printf("\t[Address in Hex (Dec) ]\t[Value]\n");
	for (address = start; address <= stop; address += 4){
		printf("\t0x%08x (%d) :\t0x%08x\n", address, address, mem_read_32(address));
	}
	printf("\n");
}

/***************************************************************/
/* Dump current values of registers to the teminal             */   
/***************************************************************/
void rdump() {                               
	int i; 
	printf("-------------------------------------\n");
	printf("Dumping Register Content\n");
	printf("-------------------------------------\n");
	printf("# Instructions Executed\t: %u\n", INSTRUCTION_COUNT);
	printf("PC\t: 0x%08x\n", CURRENT_STATE.PC);
	printf("-------------------------------------\n");
	printf("[Register]\t[Value]\n");
	printf("-------------------------------------\n");
	for (i = 0; i < RISCV_REGS; i++){
		printf("[R%d]\t: 0x%08x\n", i, CURRENT_STATE.REGS[i]);
	}
	printf("-------------------------------------\n");
	
}

/***************************************************************/
/* Read a command from standard input.                         */  
/***************************************************************/
void handle_command() {                         
	char buffer[20];
	uint32_t start, stop, cycles;
	uint32_t register_no;
	int register_value;

	printf("OZU-RISCV SIM:> ");

	if (scanf("%s", buffer) == EOF){
		exit(0);
	}

	switch(buffer[0]) {
		case 'S':
		case 's':
			runAll(); 
			break;
		case 'M':
		case 'm':
			if (scanf("%x %x", &start, &stop) != 2){
				break;
			}
			mdump(start, stop);
			break;
		case '?':
			help();
			break;
		case 'Q':
		case 'q':
			printf("**************************\n");
			printf("Exiting OZU-RISCV! Good Bye...\n");
			printf("**************************\n");
			exit(0);
		case 'R':
		case 'r':
			if (buffer[1] == 'd' || buffer[1] == 'D'){
				rdump();
			}else if(buffer[1] == 'e' || buffer[1] == 'E'){
				reset();
			}
			else {
				if (scanf("%d", &cycles) != 1) {
					break;
				}
				run(cycles);
			}
			break;
		case 'I':
		case 'i':
			if (scanf("%u %i", &register_no, &register_value) != 2){
				break;
			}
			CURRENT_STATE.REGS[register_no] = register_value;
			NEXT_STATE.REGS[register_no] = register_value;
			break;
		case 'P':
		case 'p':
			print_program(); 
			break;
		default:
			printf("Invalid Command.\n");
			break;
	}
}

/***************************************************************/
/* reset registers/memory and reload program                   */
/***************************************************************/
void reset() {   
	int i;
	/*reset registers*/
	for (i = 0; i < RISCV_REGS; i++){
		CURRENT_STATE.REGS[i] = 0;
	}
	
	for (i = 0; i < NUM_MEM_REGION; i++) {
		uint32_t region_size = MEM_REGIONS[i].end - MEM_REGIONS[i].begin + 1;
		memset(MEM_REGIONS[i].mem, 0, region_size);
	}
	
	/*load program*/
	load_program();
	
	/*reset PC*/
	INSTRUCTION_COUNT = 0;
	CURRENT_STATE.PC =  MEM_TEXT_BEGIN;
	NEXT_STATE = CURRENT_STATE;
	RUN_FLAG = TRUE;
}

/***************************************************************/
/* Allocate and set memory to zero                             */
/***************************************************************/
void init_memory() {                                           
	int i;
	for (i = 0; i < NUM_MEM_REGION; i++) {
		uint32_t region_size = MEM_REGIONS[i].end - MEM_REGIONS[i].begin + 1;
		MEM_REGIONS[i].mem = malloc(region_size);
		memset(MEM_REGIONS[i].mem, 0, region_size);
	}
}

/**************************************************************/
/* load program into memory                                   */
/**************************************************************/
void load_program() {                   
	FILE * fp;
	int i, word;
	uint32_t address;

	/* Open program file. */
	fp = fopen(prog_file, "r");
	if (fp == NULL) {
		printf("Error: Can't open program file %s\n", prog_file);
		exit(-1);
	}

	/* Read in the program. */
	i = 0;
	while( fscanf(fp, "%x\n", &word) != EOF ) {
		address = MEM_TEXT_BEGIN + i;
		mem_write_32(address, word);
		printf("writing 0x%08x into address 0x%08x (%d)\n", word, address, address);
		i += 4;
	}
	PROGRAM_SIZE = i/4;
	printf("Program loaded into memory.\n%d words written into memory.\n\n", PROGRAM_SIZE);
	fclose(fp);
}

/************************************************************/
/* decode and execute instruction                           */ 
/************************************************************/
	/*YOU NEED TO IMPLEMENT THIS*/
	/* execute one instruction at a time. Use/update CURRENT_STATE and and NEXT_STATE, as necessary.*/

uint32_t sign_extend(uint32_t imm_i, int bit_size)
{
	uint32_t mask = 1U << (bit_size - 1);
	return (imm_i ^ mask) - mask;
}

void handle_instruction(){
	/*YOU NEED TO IMPLEMENT THIS*/
	uint32_t current_ins = mem_read_32(CURRENT_STATE.PC); // Reading instruction from memory
	NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Incrementing PC by 4 because of 32-bit instructions.

	uint32_t opcode = current_ins & 0x7F;
	uint32_t rd = (current_ins >> 7) & 0x1F;
	uint32_t rs1 = (current_ins >> 15) & 0x1F;
	uint32_t rs2 = (current_ins >> 20) & 0x1F;
	uint32_t funct3 = (current_ins >> 12) & 0x7;
	uint32_t funct7 = (current_ins >> 25) & 0x7F;
	uint32_t imm_i = (current_ins >> 20) & 0xFFF;
	uint32_t shamt = (current_ins >> 20) & 0x1F;
	int32_t offsetI = ((current_ins >> 20) << 20) >> 20;
	uint32_t offsetS = ((current_ins >> 25) << 11) | ((current_ins >> 7) & 0x1F);
	uint32_t addressS = CURRENT_STATE.REGS[rs1] + sign_extend(offsetS, 12);
	uint32_t offsetSB = ((current_ins >> 31) << 11) | (((current_ins >> 7) & 0x1) << 10) | (((current_ins >> 25) & 0x3F) << 4) | (((current_ins >> 8) & 0xF) << 0);
	uint32_t immLUI = (current_ins >> 12) & 0xFFFFF; // Extract 20-bit immediate
	uint32_t immAUIPC = (current_ins >> 12) & 0xFFFFF; // Extract 20-bit immediate
	uint32_t offsetJAL = ((current_ins >> 31) << 20) | (((current_ins >> 21) & 0x3FF) << 1) | (((current_ins >> 20) & 0x1) << 11) | ((current_ins >> 12) & 0xFF) << 12; // 20 bit offset
	uint32_t offsetJALR = current_ins >> 20;

	switch (opcode) {
        case 0x33: // R-Type instructions
            switch (funct3) {
                case 0x0:
                    if (funct7 == 0x00) {
                        // ADD
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] + CURRENT_STATE.REGS[rs2];
                    } else if (funct7 == 0x20) {
                        // SUB
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] - CURRENT_STATE.REGS[rs2];
                    }
					else if (funct7 == 0x1)
					{ //Mul
						NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] * CURRENT_STATE.REGS[rs2];
					}
					break;

                case 0x1:
                    // SLL
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] << (CURRENT_STATE.REGS[rs2] & 0x1F);
                    break;
                case 0x2:
                    // SLT
                    if (CURRENT_STATE.REGS[rs1] < CURRENT_STATE.REGS[rs2])
					{
						NEXT_STATE.REGS[rd] = 1;
					}
					else{
						NEXT_STATE.REGS[rd] = 0;
					}
                    break;
                case 0x3:
                    // SLTU
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] < CURRENT_STATE.REGS[rs2];
                    break;
                case 0x4:
                    // XOR
                    if (funct7 == 0x0)
					{
							NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] ^ CURRENT_STATE.REGS[rs2];
					}
					else if (funct7 == 0x1) //Div
					{
						NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] / CURRENT_STATE.REGS[rs2];
					}
		
                    break;
                case 0x5:
                    if (funct7 == 0x00) {
                        // SRL
                        NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] >> (CURRENT_STATE.REGS[rs2] & 0x1F);
                    } else if (funct7 == 0x20) {
                        // SRA
                        NEXT_STATE.REGS[rd] = (int32_t)CURRENT_STATE.REGS[rs1] >> (CURRENT_STATE.REGS[rs2] & 0x1F);
                    }
                    break;
                case 0x6:
                    // OR
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] | CURRENT_STATE.REGS[rs2];
                    break;
                case 0x7:
                    // AND
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] & CURRENT_STATE.REGS[rs2];
                    break;
                default:
                    printf("Unknown R-type function\n");
                    break;
            }
            break;

			
		case 0x13: // I-Type instructions
			switch (funct3) {
    			case 0x0: // ADDI
        			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] + sign_extend(imm_i, 12);
        			break;
    			case 0x1: // SLLI
        			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] << (imm_i & 0x1F);
     			   break;
   				case 0x2: // SLTI
        			NEXT_STATE.REGS[rd] = (CURRENT_STATE.REGS[rs1] < sign_extend(imm_i, 12)) ? 1 : 0;
        			break;
    			case 0x3: // SLTIU
        			NEXT_STATE.REGS[rd] = ((uint32_t)CURRENT_STATE.REGS[rs1] < imm_i) ? 1 : 0;
        			break;
    			case 0x4: // XORI
        			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] ^ sign_extend(imm_i, 12);
        			break;
    			case 0x5: // SRLI or SRAI
        			if ((imm_i & 0x400) == 0) { // SRLI
        			    NEXT_STATE.REGS[rd] = (uint32_t)CURRENT_STATE.REGS[rs1] >> (imm_i & 0x1F);
       				} else { // SRAI
        			    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] >> (imm_i & 0x1F);
        			}
        			break;
    			case 0x6: // ORI
        			NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] | sign_extend(imm_i, 12);
        			break;
    			case 0x7: // ANDI
     			   NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs1] & sign_extend(imm_i, 12);
     			   break;
    			default:
        			printf("Unknown I-type instruction with funct3: 0x%X\n", funct3);
        			break;
			}
			break; // End of case 0x13

		case 0x3:// I- Format Load Instructions
			switch (funct3) {

				case 0x0: // lb: load byte
					NEXT_STATE.REGS[rd] = (int8_t)mem_read_32(CURRENT_STATE.REGS[rs1] + offsetI);
					break;

				case 0x1: // lb: load byte
					NEXT_STATE.REGS[rd] = (int16_t)mem_read_32(CURRENT_STATE.REGS[rs1] + sign_extend(offsetI, 12));
					break;

				case 0x2: // lw: load word
					NEXT_STATE.REGS[rd] = (int32_t)mem_read_32(CURRENT_STATE.REGS[rs1] + offsetI);
					break;

				case 0x4: // lbu: load byte unsigned
					NEXT_STATE.REGS[rd] = mem_read_32(CURRENT_STATE.REGS[rs1] + sign_extend(offsetI, 12)) & 0xFF;
					break;

				case 0x5: // lhu: load halfword unsigned
					NEXT_STATE.REGS[rd] = mem_read_32(CURRENT_STATE.REGS[rs1] + sign_extend(offsetI, 12)) & 0xFFFF;
					break;
				default:
        			printf("Unknown I-type instruction with funct3: 0x%X\n", funct3);
        			break;
			}
			break; // End of case 0x3

		case 0x23: // S-Type store instructions
			
			switch (funct3) {
        		case 0x0: // SB: Store Byte
            		mem_write_32(addressS, CURRENT_STATE.REGS[rs2] & 0xFF);
            		break;
        		case 0x1: // SH: Store Halfword
            		mem_write_32(addressS, CURRENT_STATE.REGS[rs2] & 0xFFFF);
            		break;
        		case 0x2: // SW: Store Word
        
					mem_write_32(addressS, CURRENT_STATE.REGS[rs2]);
            		break;
        		default:
            		printf("Unknown S-type instruction with funct3: 0x%X\n", funct3);
            		break;
			}
			break; // End of case 0x23

		case 0x63: // SB-Type branch instructions
			switch (funct3) {
			case 0x0: // BEQ: Branch if Equal
				if (CURRENT_STATE.REGS[rs1] == CURRENT_STATE.REGS[rs2])
				{
					NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetSB, 12);
				}

			case 0x1: // BNE: Branch if Not Equal
				if (CURRENT_STATE.REGS[rs1] != CURRENT_STATE.REGS[rs2])
				{
					NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetSB, 12);
				}

			case 0x4: // BLT: Branch if Less Than
				if (CURRENT_STATE.REGS[rs1] < CURRENT_STATE.REGS[rs2])
				{
					NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetSB, 13);
				}

			case 0x5: // BGE: Branch if Greater or Equal
				if (CURRENT_STATE.REGS[rs1] >= CURRENT_STATE.REGS[rs2])
				{
					NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetSB, 12);
				}

			case 0x6: // BLTU: Branch if Less Than Unsigned
				if (CURRENT_STATE.REGS[rs1] < (uint32_t)CURRENT_STATE.REGS[rs2])
				{
					NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetSB, 12);
				}

			case 0x7: // BGEU: Branch if Greater or Equal Unsigned
				if (CURRENT_STATE.REGS[rs1] >= (uint32_t)CURRENT_STATE.REGS[rs2])
				{
					NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetSB, 12);
				}

			default:
            	printf("Unknown SB-type instruction with funct3: 0x%X\n", funct3);
            	break;
			}
			break; // End of case 0x63

		// U-TYPE INSTRUCTIONS
		case 0x37: // lui
			
    		NEXT_STATE.REGS[rd] = sign_extend(immLUI << 12, 20); // Shift left by 12 bits 
			break; 

		case 0x17: // auipc
			
			NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + sign_extend(immAUIPC << 12, 20); // Shift left by 12 bits and add to PC
			break;
		
		// UJ-TYPE INSTRUCTIONS
		case 0x6F: // jal
			NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
			NEXT_STATE.PC = CURRENT_STATE.PC + sign_extend(offsetJAL, 20);
			break;
		
		case 0x67: // jalr
			NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
			NEXT_STATE.PC = (CURRENT_STATE.REGS[rs1] + sign_extend(offsetJALR, 12)) & 0xFFFFFFFE;
			break;
			
		// SYSTEM INSTRUCTIONS
		case 0x73:
    		if (funct3 == 0x0) {
        		if (current_ins == 0x00000073) {
            		// Ecall instruction
            		RUN_FLAG = FALSE; // Sistemi durdur, ecall çağrısı
        		} else if (current_ins == 0x00000013) {
            	// Nop instruction
            	// Nop işlemi için herhangi bir işlem yapılmasına gerek yok,
            	// çünkü bu bir "no operation" işlemidir ve herhangi bir değişiklik yapmaz.
            	// Bu kod bloğu okunabilirlik için burada bulunmaktadır.
        		}
    		}
			break;
    break;
	}
}


/************************************************************/
/* Initialize Memory                                        */ 
/************************************************************/
void initialize() { 
	init_memory();
	CURRENT_STATE.PC = MEM_TEXT_BEGIN;
	NEXT_STATE = CURRENT_STATE;
	RUN_FLAG = TRUE;
}

/**********************************************************************/
/* Print the program loaded into memory (in RISC-V assembly format)   */ 
/**********************************************************************/
void print_program(){
	int i;
	uint32_t addr;
	
	for(i=0; i<PROGRAM_SIZE; i++){
		addr = MEM_TEXT_BEGIN + (i*4);
		printf("[0x%x]\t", addr);
		print_instruction(addr);
	}
}


/******************************************************************************/
/* Print the instruction at given memory address (in RISC-V assembly format)  */
/******************************************************************************/
void print_instruction(uint32_t addr)
{
	uint32_t instruction = mem_read_32(addr);
	uint32_t opcode = instruction & 0x7F;
	uint32_t rd = (instruction >> 7) & 0x1F;
	uint32_t rs1 = (instruction >> 15) & 0x1F;
	uint32_t rs2 = (instruction >> 20) & 0x1F;
	uint32_t funct3 = (instruction >> 12) & 0x7;
	uint32_t funct7 = (instruction >> 25) & 0x7F;
	uint32_t imm_i = (instruction >> 20) & 0xFFF;
	uint32_t shamt = (instruction >> 20) & 0x1F;
	uint32_t offsetI = (instruction >> 20) & 0xFFF;
	uint32_t offsetS = ((instruction >> 25) << 11) | ((instruction >> 7) & 0x1F);
	uint32_t offsetSB = ((instruction >> 31) << 12) | (((instruction >> 7) & 0x1) << 11) | (((instruction >> 25) & 0x3F) << 5) | (((instruction >> 8) & 0xF) << 1);
	uint32_t imm_u = (instruction >> 12) & 0xFFFFF;
	uint32_t offsetJAL = ((instruction >> 31) << 20) | (((instruction >> 21) & 0x3FF) << 1) | (((instruction >> 20) & 0x1) << 11) | ((instruction >> 12) & 0xFF) << 12;
	uint32_t offsetJALR = (instruction >> 20) & 0xFFF;


	switch (opcode) {
		case 0x33: // R-Type Instructions
			if (funct3 == 0x0)
			{
				if (funct7 == 0x0) // add
				{
					printf("add\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
				else if (funct7 == 0x20) // sub
				{
					printf("sub\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
				else if (funct7 == 0x1) // mul
				{
					printf("mul\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
			}

			else if (funct3 == 0x1)
			{
				if (funct7 == 0x0) // sll
				{
					printf("sll\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
			}

			else if (funct3 == 0x2) // slt
			{
				printf("slt\tx%d, x%d, x%d\n", rd, rs1, rs2);
			}

			else if (funct3 == 0x3) // sltu
			{
				printf("sltu\tx%d, x%d, x%d\n", rd, rs1, rs2);
			}

			else if (funct3 == 0x4)
			{
				if (funct7 == 0x0) // xor
				{
					printf("xor\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
				else if (funct7 == 0x1) // div
				{
					printf("div\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
			}

			else if (funct3 == 0x5)
			{
				if (funct7 == 0x0) // srl
				{
					printf("srl\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
				else if (funct7 == 0x20) // sra
				{
					printf("sra\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
				else if (funct7 == 0x1) // divu
				{
					printf("divu\tx%d, x%d, x%d\n", rd, rs1, rs2);
				}
			}

			else if (funct3 == 0x6) // or
			{
				printf("or\tx%d, x%d, x%d\n", rd, rs1, rs2);
			}

			else if (funct3 == 0x7) // and
			{
				printf("and\tx%d, x%d, x%d\n", rd, rs1, rs2);
			}
			break;
	
		case 0x13: // I-Type Instructions
			
			if (funct3 == 0x0) // addi
			{
				printf("addi\tx%d, x%d, %d\n", rd, rs1, sign_extend(imm_i, 12));
			}

			else if (funct3 == 0x1) // slli
			{
				printf("slli\tx%d, x%d, %d\n", rd, rs1, shamt);
			}

			else if (funct3 == 0x2) // slti
			{
				printf("slti\tx%d, x%d, %d\n", rd, rs1, sign_extend(imm_i, 12));
			}

			else if (funct3 == 0x3) // sltiu
			{
				printf("sltiu\tx%d, x%d, %d\n", rd, rs1, sign_extend(imm_i, 12));
			}

			else if (funct3 == 0x4) // xori
			{
				printf("xori\tx%d, x%d, %d\n", rd, rs1, sign_extend(imm_i, 12));
			}

			else if (funct3 == 0x5)
			{
				if (imm_i >> 5 == 0x0) // srli
				{
					printf("srli\tx%d, x%d, %d\n", rd, rs1, shamt);
				}
				else if (imm_i >> 5 == 0x20) // srai
				{
					printf("srai\tx%d, x%d, %d\n", rd, rs1, shamt);
				}
			}

			else if (funct3 == 0x6) // ori
			{
				printf("ori\tx%d, x%d, %d\n", rd, rs1, sign_extend(imm_i, 12));
			}

			else if (funct3 == 0x7) // andi
			{
				printf("andi\tx%d, x%d, %d\n", rd, rs1, sign_extend(imm_i, 12));
			}
			break;

		case 0x3:
			if (funct3 == 0x0) // lb
			{
				printf("lb\tx%d, %d(x%d)\n", rd, sign_extend(offsetI, 12), rs1);
			}

			else if (funct3 == 0x1) // lh
			{
				printf("lh\tx%d, %d(x%d)\n", rd, sign_extend(offsetI, 12), rs1);
			}

			else if (funct3 == 0x2) // lw
			{
				printf("lw\tx%d, %d(x%d)\n", rd, sign_extend(offsetI, 12), rs1);
			}

			else if (funct3 == 0x4) // lbu
			{
				printf("lbu\tx%d, %d(x%d)\n", rd, sign_extend(offsetI, 12), rs1);
			}

			else if (funct3 == 0x5) // lhu
			{
				printf("lhu\tx%d, %d(x%d)\n", rd, sign_extend(offsetI, 12), rs1);
			}
			break;

		case 0x23: // S-Type Instructions
			if (funct3 == 0x0) // sb
			{
				printf("sb\tx%d, %d(x%d)\n", rs2, sign_extend(offsetS, 12), rs1);
			}

			else if (funct3 == 0x1) // sh
			{
				printf("sh\tx%d, %d(x%d)\n", rs2, sign_extend(offsetS, 12), rs1);
			}

			else if (funct3 == 0x2) // sw
			{
				printf("sw\tx%d, %d(x%d)\n", rs2, sign_extend(offsetS, 12), rs1);
			}
			break;

		case 0x63: // SB-Type Instructions
			
			if (funct3 == 0x0) // beq
			{
				printf("beq\tx%d, x%d, %d\n", rs1, rs2, sign_extend(offsetSB, 13));
			}

			else if (funct3 == 0x1) // bne
			{
				printf("bne\tx%d, x%d, %d\n", rs1, rs2, sign_extend(offsetSB, 13));
			}

			else if (funct3 == 0x4) // blt
			{
				printf("blt\tx%d, x%d, %d\n", rs1, rs2, sign_extend(offsetSB, 13));
			}

			else if (funct3 == 0x5) // bge
			{
				printf("bge\tx%d, x%d, %d\n", rs1, rs2, sign_extend(offsetSB, 13));
			}

			else if (funct3 == 0x6) // bltu
			{
				printf("bltu\tx%d, x%d, %d\n", rs1, rs2, sign_extend(offsetSB, 13));
			}

			else if (funct3 == 0x7) // bgeu
			{
				printf("bgeu\tx%d, x%d, %d\n", rs1, rs2, sign_extend(offsetSB, 13));
			}
			break;

		case 0x37: // lui
			
			printf("lui\tx%d, %d\n", rd, sign_extend(imm_u, 20));
			break;

		case 0x17: // auipc
			
			printf("auipc\tx%d, %d\n", rd, sign_extend(imm_u, 20));
			break;

		case 0x6F: // jal
			printf("jal\tx%d, %d\n", rd, sign_extend(offsetJAL, 20));
			break;

		case 0x67: // jalr
			
			printf("jalr\tx%d, x%d, %d\n", rd, rs1, sign_extend(offsetJALR, 12));
			break;

		case 0x73:// ecall
			if (funct3 == 0x0 && instruction == 0x00000073)
			{
			printf("ecall\n");
			}
			else if (funct3 == 0x0 && instruction == 0x00000013)
			{
			printf("nop\n");
			}
			break;


		default:
			printf("Bilemedim");
	
	}
}


/***************************************************************/
/* main()                                                      */
/***************************************************************/
int main(int argc, char *argv[]) {                              
	printf("\n********************************\n");
	printf("Welcome to OZU-RISCV SIMULATOR...\n");
	printf("*********************************\n\n");
	
	if (argc < 2) {
		printf("Error: You should provide input file.\nUsage: %s <input program> \n\n",  argv[0]);
		exit(1);
	}

	strcpy(prog_file, argv[1]);
	initialize();
	load_program();
	help();
	while (1){
		handle_command();
	}
	return 0;
}


// Print soruna bak fonksiyonları caselerin içine taşı