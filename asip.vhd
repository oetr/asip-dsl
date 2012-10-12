library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instructions_lib.all;
----------------------------------------------------------------------
entity asip is
  port (
    clk   : in  std_logic;
    reset : in  std_logic;
    oLEDR : out std_logic_vector (17 downto 0);
    );
end entity asip;
----------------------------------------------------------------------
architecture arch of asip is
  --------------------------------------------------------------------
  -- ASIP
  --------------------------------------------------------------------
  -- constants
  constant INSTR_WIDTH : integer := 4;
  -- prepare: 16 32-bit registers
  constant REG_WIDTH   : integer := 32;
  constant REG_NUM     : integer := 16;
  type reg_type is array (natural range 0 to REG_NUM-1) of
    std_logic_vector (REG_WIDTH-1 downto 0);
  -- declare: 16 32-bit registers
  signal registers_reg  : reg_type;
  signal registers_next : reg_type;
  
begin
  --------------------------------------------------------------------
  -- Clock event update and default assignments
  --------------------------------------------------------------------
  process (clk, reset) is
  begin
    if reset = '1' then
      registers_reg <= (others => (others => '0'));
    elsif rising_edge(clk) then
      registers_reg <= registers_next;
    end if;
  end process;

  --------------------------------------------------------------------
  -- working through the instructions
  --------------------------------------------------------------------
  process is
  begin
    
  end process;

  --------------------------------------------------------------------
  -- Output
  --------------------------------------------------------------------
  -- connect first register to the LEDs
  oLEDR <= registers_reg(0)(17 downto 0);
  
end architecture arch;
