/*************************************************************************************|
|   1. YOU ARE NOT ALLOWED TO SHARE/PUBLISH YOUR CODE (e.g., post on piazza or online)|
|   2. Fill memory_hierarchy.c                                                        |
|   3. Do not use any other .c files neither alter mipssim.h or parser.h              |
|   4. Do not include any other library files                                         |
|*************************************************************************************/

#include "mipssim.h"

struct cache_block {
    int valid_bit;
    int tag_bit;
    int order;
    int block0;
    int block1;
    int block2;
    int block3;
};



// Cache helper functions
int get_index(int addr);
int get_tag(int addr);
int get_block(int addr);
int get_item(int block_num, struct cache_block *block);
void set_block(int tag, int addr, struct cache_block *block);

uint32_t cache_type = 0;
struct cache_block *cache_memory;
int LRU_order_num;
int tagbits, indexbits, blocks;

#define ADDRESS 32
#define BLOCK 4

int get_index(int addr) {
    return (addr >> BLOCK) & ((1 << indexbits) - 1);
}

int get_tag(int addr) {
    return addr >> (BLOCK + indexbits);
}

int get_block(int addr) {
    return addr & ((1 << BLOCK) - 1);
}

void set_block(int tag, int addr, struct cache_block *block) {
    block->valid_bit = 1;
    block->tag_bit = tag;
    block->order = LRU_order_num++;
    int start_addr = addr / 4 * 4;
    block->block0 = arch_state.memory[start_addr];
    block->block1 = arch_state.memory[start_addr+1];
    block->block2 = arch_state.memory[start_addr+2];
    block->block3 = arch_state.memory[start_addr+3];
}

void memory_state_init(struct architectural_state *arch_state_ptr) {
    arch_state_ptr->memory = (uint32_t *) malloc(sizeof(uint32_t) * MEMORY_WORD_NUM);
    memset(arch_state_ptr->memory, 0, sizeof(uint32_t) * MEMORY_WORD_NUM);
    if (cache_size == 0) {
        // CACHE DISABLED
        memory_stats_init(arch_state_ptr, 0); // WARNING: we initialize for no cache 0
    } else { 
        /// @students: memory_stats_init(arch_state_ptr, X); <-- fill # of tag bits for cache 'X' correctly
        cache_memory = (struct cache_block *) malloc(cache_size / 16 * sizeof(struct cache_block));
        blocks = cache_size / 16;
        memset(cache_memory, 0, cache_size / 16 * sizeof(struct cache_block) );
        LRU_order_num = 0;
        
        switch(cache_type) {
            case CACHE_TYPE_DIRECT: // direct mapped
                tagbits = ADDRESS - log2(cache_size);
                indexbits = log2(cache_size) - BLOCK;
                break;
            case CACHE_TYPE_FULLY_ASSOC: // fully associative
                tagbits = ADDRESS - BLOCK;
                indexbits = 0;
                break;
            case CACHE_TYPE_2_WAY: // 2-way associative
                tagbits = ADDRESS - log2(cache_size) + 1;
                indexbits = log2(cache_size) - BLOCK - 1;
                break;
        }
        memory_stats_init(arch_state_ptr, tagbits);
        printf("Index: %d, Tag: %d", indexbits, tagbits);
    }
}

// returns data on memory[address / 4]
int memory_read(int address){
    arch_state.mem_stats.lw_total++;
    check_address_is_word_aligned(address);

    if (cache_size == 0) {
        // CACHE DISABLED
        return (int) arch_state.memory[address / 4];
    } else {
        // CACHE ENABLED
        
        /// @students: your implementation must properly increment: arch_state_ptr->mem_stats.lw_cache_hits
        int index = get_index(address);
        int tag = get_tag(address);
        int block = get_block(address);
        
        switch(cache_type) {
            case CACHE_TYPE_DIRECT: // direct mapped
                if (cache_memory[index].valid_bit && cache_memory[index].tag_bit == tag) {
                    arch_state.mem_stats.lw_cache_hits++;
                } else {
                    set_block(tag, address / 4, &cache_memory[index]);
                }
                break;
            case CACHE_TYPE_FULLY_ASSOC: // fully associative
            {
                for (int i = 0; i < blocks; i++) {
                    if (cache_memory[i].valid_bit && cache_memory[i].tag_bit == tag) {
                        arch_state.mem_stats.lw_cache_hits++;
                        cache_memory[i].order = LRU_order_num++;
                        return (int) arch_state.memory[address / 4];
                    }
                }
                for (int i = 0; i < blocks; i++) {
                    if (!cache_memory[i].valid_bit) {
                        set_block(tag, address / 4, &cache_memory[i]);
                        return (int) arch_state.memory[address / 4];
                    }
                }
                int min = cache_memory[0].order; int min_i = 0;
                for (int i = 1; i < blocks; i++) {
                    if (cache_memory[i].order < min) {
                        min = cache_memory[i].order;
                        min_i = i;
                    }
                }
                set_block(tag, address / 4, &cache_memory[min_i]);
                break;
            }
            case CACHE_TYPE_2_WAY: // 2-way associative 
            {
                int first_slot = cache_memory[index*2].valid_bit & (cache_memory[index*2].tag_bit == tag);
                int second_slot = cache_memory[index*2+1].valid_bit & (cache_memory[index*2+1].tag_bit == tag);
                if (first_slot || second_slot) {
                    arch_state.mem_stats.lw_cache_hits++;
                    if (first_slot) 
                        cache_memory[index*2].order = LRU_order_num++;
                    else 
                        cache_memory[index*2+1].order = LRU_order_num++;
                } else if (!cache_memory[index*2].valid_bit) {
                    set_block(tag, address / 4, &cache_memory[index*2]);
                } else if (!cache_memory[index*2+1].valid_bit) {
                    set_block(tag, address / 4, &cache_memory[index*2+1]);
                } else {
                    if (cache_memory[index*2].order < cache_memory[index*2+1].order) 
                        set_block(tag, address / 4, &cache_memory[index*2]);
                    else 
                        set_block(tag, address / 4, &cache_memory[index*2+1]);
                }
                break;
            }
        }
        return (int) arch_state.memory[address / 4];
    }
    return 0;
}

// writes data on memory[address / 4]
void memory_write(int address, int write_data) {
    arch_state.mem_stats.sw_total++;
    check_address_is_word_aligned(address);

    if (cache_size == 0) {
        // CACHE DISABLED
        arch_state.memory[address / 4] = (uint32_t) write_data;
    } else {
        // CACHE ENABLED
        
        /// @students: your implementation must properly increment: arch_state_ptr->mem_stats.sw_cache_hits
        int index = get_index(address);
        int tag = get_tag(address);
        int block = get_block(address);

        switch(cache_type) {
        case CACHE_TYPE_DIRECT: // direct mapped
            if (cache_memory[index].valid_bit && cache_memory[index].tag_bit == tag) {
                arch_state.mem_stats.sw_cache_hits++;
                arch_state.memory[address / 4] = (uint32_t) write_data;
                set_block(tag, address / 4, &cache_memory[index]);
            } else {
                arch_state.memory[address / 4] = (uint32_t) write_data;
            }
            break;
        case CACHE_TYPE_FULLY_ASSOC: // fully associative
            for (int i = 0; i < blocks; i++) {
                if (cache_memory[i].valid_bit && cache_memory[i].tag_bit == tag) {
                    arch_state.mem_stats.sw_cache_hits++;
                    arch_state.memory[address / 4] = (uint32_t) write_data;
                    set_block(tag, address / 4, &cache_memory[i]);
                    return;
                }
            }
            arch_state.memory[address / 4] = (uint32_t) write_data;
            break;
        case CACHE_TYPE_2_WAY: // 2-way associative
            {
            int first_slot = cache_memory[index*2].valid_bit && (cache_memory[index*2].tag_bit == tag);
            int second_slot = cache_memory[index*2+1].valid_bit && (cache_memory[index*2+1].tag_bit == tag);
            if (first_slot || second_slot) {
                arch_state.mem_stats.sw_cache_hits++;
                arch_state.memory[address / 4] = (uint32_t) write_data;
                if (first_slot)
                    set_block(tag, address / 4, &cache_memory[index*2]);
                else 
                    set_block(tag, address / 4, &cache_memory[index*2+1]);
            } else {
                arch_state.memory[address / 4] = (uint32_t) write_data;
            }
            break;
            } 
        }
    }
}
