library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
----------------------------------------------------------------------
package test is
  --------------------------------------------------------------------
  -- Constants
  --------------------------------------------------------------------
constant OPERATION_WIDTH : integer := 4;
constant REGISTER_WIDTH : integer := 32;
constant REGISTER_N_WIDTH : integer := 4;
constant LINE_N_WIDTH : integer := 10;
constant INSTRUCTION_WIDTH : integer := 48;

  --------------------------------------------------------------------
  -- Opcodes
  --------------------------------------------------------------------
constant asip_jump : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0010";
constant asip_wait : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0001";
constant asip_jump_if_true : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0011";
constant asip_add_rvr : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0100";
constant asip_halt : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0110";
constant asip_eq_rv : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0101";
constant asip_set_rv : std_logic_vector(OPERATION_WIDTH-1 downto 0) := "0000";
  --------------------------------------------------------------------
  -- Registers
  --------------------------------------------------------------------
subtype register_type is std_logic_vector(REGISTER_WIDTH-1 downto 0);
type regs_type is array (natural range 0 to 2**REGISTER_N_WIDTH-1) of
 register_type;

  --------------------------------------------------------------------
  -- Code
  --------------------------------------------------------------------
constant INSTRUCTION_COUNT : integer := 7;
type instruction_rom_type is array (0 to INSTRUCTION_COUNT - 1) of
std_logic_vector (INSTRUCTION_WIDTH-1 downto 0);
constant INSTRUCTIONS : instruction_rom_type :=
("000000000000000000000000000000000000000000000000",
"000000000000000000101111101011110000100000000001",
"000000000000000000000000000000000000000100000100",
"000000000000000000000000000000000000101000000101",
"000000000000000000000000000000000000000001100011",
"000000000000000000000000000000000000000000010010",
"000000000000000000000000000000000000000000000110");

end package test;
