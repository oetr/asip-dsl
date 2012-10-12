library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
----------------------------------------------------------------------
package instructions_lib is

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
  type reg_type is array (natural range 0 to REG_NUM-1) of
    std_logic_vector (REG_WIDTH-1 downto 0);

  constant INSTR_WIDTH : integer := INSTR_NAME_WIDTH + REG_WIDTH + REG_NUM_BITS;

  --------------------------------------------------------------------
  -- User instructions
  --------------------------------------------------------------------
  constant INSTRUCTION_COUNT : integer := 4;
  type instruction_rom_type is array (0 to INSTRUCTION_COUNT - 1) of
    std_logic_vector (39 downto 0);
  constant INSTRUCTIONS : instruction_rom_type :=
    (
      "0000000000000000000000000000000000000000",
      "0000000000101111101011110000100000000001",
      "1111111111111111111111111111111100000000",
      "0000000000101111101011110000100000000001"
      );

end package instructions_lib;
