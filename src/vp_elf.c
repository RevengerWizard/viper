/*
** vp_elf.c
** ELF emitter
*/

#include "vp_buf.h"
#include "vp_state.h"
#include "vp_codegen.h"

/*
** ELF constants
*/
#define EI_NIDENT       16
#define ET_EXEC         2
#define EM_X86_64       62
#define EV_CURRENT      1
#define ELFCLASS64      2
#define ELFDATA2LSB     1
#define PT_LOAD         1
#define PF_X            1
#define PF_R            4
#define SHT_PROGBITS    1
#define SHF_ALLOC       2
#define SHF_EXECINSTR   4

static void elf_build(VpState* V, SBuf* sb)
{
    size_t codesize = sbuf_len(&V->code);

    Code* code = vp_tab_get(&V->funcs, vp_str_newlen("main"));
    vp_assertX(code, "?");
    int32_t entryofs = code->ofs;

    /* ELF header */
    char* p = vp_buf_need(sb, 64);

    /* e_ident */
    p[0] = 0x7f; p[1]= 'E'; p[2] = 'L'; p[3] = 'F'; /* ELF magic */
    p[4] = ELFCLASS64;  /* 64 bit */
    p[5] = ELFDATA2LSB; /* Little endian */
    p[6] = EV_CURRENT;  /* ELF version */
    p[7] = 0;   /* System V ABI */
    memset(p + 8, 0, 8);    /* ABI version + padding */

    /* ELF header fields */
    *(uint16_t*)(&p[16]) = ET_EXEC;     /* e_type = executable */
    *(uint16_t*)(&p[18]) = EM_X86_64;   /* e_machine = x86-64 */
    *(uint32_t*)(&p[20]) = 1;   /* e_version */
    *(uint64_t*)(&p[24]) = 0x400000 + 0x1000 + entryofs; /* e_entry */
    *(uint64_t*)(&p[32]) = 64;  /* e_phoff = program header offset */
    *(uint64_t*)(&p[40]) = 64 + 56; /* e_shoff = section header offset */
    *(uint32_t*)(&p[48]) = 0;   /* e_flags */
    *(uint16_t*)(&p[52]) = 64;  /* e_ehsize = ELF header size */
    *(uint16_t*)(&p[54]) = 56;  /* e_phentsize = program header entry size */
    *(uint16_t*)(&p[56]) = 1;   /* e_phnum = number of program headers */
    *(uint16_t*)(&p[58]) = 64;  /* e_shentsize = section header entry size */
    *(uint16_t*)(&p[60]) = 3;   /* e_shnum = number of section headers */
    *(uint16_t*)(&p[62]) = 2;   /* e_shstrndx = string table section index */

    sb->w = p + 64;

    /* Program header */
    *(uint32_t*)(&p[0]) = 1;    /* p_type = loadable segment */
    *(uint32_t*)(&p[4]) = PF_R | PF_X;          /* p_flags = readable + executable */
    *(uint64_t*)(&p[8]) = 0;                    /* p_offset = file offset */
    *(uint64_t*)(&p[16]) = 0x400000;            /* p_vaddr = virtual address */
    *(uint64_t*)(&p[24]) = 0x400000;            /* p_paddr = physical address */
    *(uint64_t*)(&p[32]) = 0x1000 + codesize;   /* p_filesz = size in file */
    *(uint64_t*)(&p[40]) = 0x1000 + codesize;   /* p_memsz = size in memory */
    *(uint64_t*)(&p[48]) = 0x1000;              /* p_align = alignment */

    sb->w = p + 56;

    /* Section headers */

    /* Section 0: NULL section */
    p = vp_buf_more(sb, 64);
    memset(p, 0, 64);
    sb->w = p + 64;

    /* Section 1: .text section */
    p = vp_buf_more(sb, 64);
    *(uint32_t*)(&p[0]) = 1;                /* sh_name = string table offset */
    *(uint32_t*)(&p[4]) = SHT_PROGBITS;     /* sh_type = program data */
    *(uint64_t*)(&p[8]) = SHF_ALLOC | SHF_EXECINSTR; /* sh_flags */
    *(uint64_t*)(&p[16]) = 0x400000 + 0x1000; /* sh_addr = virtual address */
    *(uint64_t*)(&p[24]) = 0x1000;          /* sh_offset = file offset */
    *(uint64_t*)(&p[32]) = codesize;        /* sh_size */
    *(uint32_t*)(&p[40]) = 0;               /* sh_link */
    *(uint32_t*)(&p[44]) = 0;               /* sh_info */
    *(uint64_t*)(&p[48]) = 1;               /* sh_addralign */
    *(uint64_t*)(&p[56]) = 0;               /* sh_entsize */
    sb->w = p + 64;

    /* Section 2: .shstrtab (string table) */
    p = vp_buf_more(sb, 64);
    *(uint32_t*)(&p[0]) = 7;                /* sh_name = string table offset */
    *(uint32_t*)(&p[4]) = 3;                /* sh_type = SHT_STRTAB */
    *(uint64_t*)(&p[8]) = 0;                /* sh_flags */
    *(uint64_t*)(&p[16]) = 0;               /* sh_addr */
    *(uint64_t*)(&p[24]) = 0x1000 + codesize; /* sh_offset */
    *(uint64_t*)(&p[32]) = 17;              /* sh_size = string table size */
    *(uint32_t*)(&p[40]) = 0;               /* sh_link */
    *(uint32_t*)(&p[44]) = 0;               /* sh_info */
    *(uint64_t*)(&p[48]) = 1;               /* sh_addralign */
    *(uint64_t*)(&p[56]) = 0;               /* sh_entsize */
    sb->w = p + 64;

    /* Pad to page boundary (4096 bytes) */
    size_t padsize = 0x1000 - sbuf_len(sb);
    if(padsize > 0)
    {
        char* pad = vp_buf_more(sb, padsize);
        memset(pad, 0, padsize);
        sb->w = pad + padsize;
    }

    /* .text section data */
    p = vp_buf_more(sb, codesize);
    memcpy(p, V->code.b, codesize);
    sb->w = p + codesize;

    /* String table */
    p = vp_buf_more(sb, 17);
    p[0] = '\0';                /* Index 0: empty string */
    memcpy(p + 1, ".text", 5);  /* Index 1: ".text" */
    p[6] = '\0';
    memcpy(p + 7, ".shstrtab", 9); /* Index 7: ".shstrtab" */
    p[16] = '\0';
    sb->w = p + 17;
}

void vp_emit_elf(VpState* V, SBuf* sb)
{
    elf_build(V, sb);
}