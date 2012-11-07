library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
----------------------------------------------------------------------
package instructions_lib is

  -- TODO: try this out later on---make the machine code easier to understand
  --type opcodes is (add, sub, addu, subu, jmp, breq, brne, ld, st);
  --type reg_number is range 0 to 31;
  --constant r0 : reg_number := 0;
  --constant r1 : reg_number := 1;


  --type instruction is record
  --  opcode                             : opcodes;
  --  source_reg1, source_reg2, dest_reg : reg_number;
  --  displacement                       : integer;
  --end record instruction;


  --type word is record
  --  instr : instruction;
  --  data  : bit_vector(31 downto 0);
  --end record word;

  --------------------------------------------------------------------
  -- Instructions
  --------------------------------------------------------------------
  constant INSTR_NAME_WIDTH : integer := 4;

  --------------------------------------------------------------------
  -- 16 32-bit registers
  --------------------------------------------------------------------
  constant REG_WIDTH    : integer := 32;
  constant REG_NUM      : integer := 16;
  constant REG_NUM_BITS : integer := 4;
  subtype register_type is std_logic_vector(REG_WIDTH-1 downto 0);
  type regs_type is array (natural range 0 to REG_NUM-1) of register_type;

  constant INSTR_WIDTH : integer := INSTR_NAME_WIDTH + REG_WIDTH + REG_NUM_BITS;

  --------------------------------------------------------------------
  -- User instructions
  --------------------------------------------------------------------
  constant INSTRUCTION_COUNT : integer := 7;
  type instruction_rom_type is array (0 to INSTRUCTION_COUNT - 1) of
    std_logic_vector (INSTR_WIDTH-1 downto 0);
  
  constant INSTRUCTIONS : instruction_rom_type :=
    ("0000000000000000000000000000000000000000",
     "0000000000101111101011110000100000000001",
     "0000000000000000000000000000000100001011",
     "0000000000000000000000000000101000000101",
     "0000000000000000000000000000000001101100",
     "0000000000000000000000000000000000010010",
     "0000000000000000000000000000000000001111");

end package instructions_lib;
