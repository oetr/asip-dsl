library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test.all;
----------------------------------------------------------------------
entity main is
port (
iCLK_50 : in std_logic;
iKEY : in std_logic_vector(3 downto 0);
oLEDR : out std_logic_vector(17 downto 0);
oLEDG : out std_logic_vector(7 downto 0)
);
end entity main;
----------------------------------------------------------------------
architecture arch of main is
  --------------------------------------------------------------------
  -- Signals
  --------------------------------------------------------------------
  signal reset, clk     : std_logic;
  signal registers_reg  : regs_type;
  signal registers_next : regs_type;
signal pc_reg : integer range 0 to 1023;
signal pc_next : integer range 0 to 1023;
signal started_reg : std_logic;
signal started_next : std_logic;
signal counter_reg : integer;
signal counter_next : integer;
signal if_reg : std_logic;
signal if_next : std_logic;
begin
  reset <= not iKEY(0);
  clk   <= iCLK_50;
  --------------------------------------------------------------------
  -- Rising edge process
  --------------------------------------------------------------------
  process (clk, reset) is
  begin
    if reset = '1' then
      registers_reg    <= (others => (others => '0'));
      pc_reg    <= 0;
      -- user registers
started_reg <= '0';
counter_reg <= 0;
if_reg <= '0';
      elsif rising_edge(clk) then
      registers_reg    <= registers_next;
      pc_reg    <= pc_next;
started_reg <= started_next;
counter_reg <= counter_next;
if_reg <= if_next;
    end if;
  end process;
--------------------------------------------------------------------
-- Instruction interpreter
--------------------------------------------------------------------
process (registers_reg, pc_reg, started_reg, counter_reg, if_reg) is
variable instruction      : std_logic_vector(INSTRUCTION_WIDTH-1 downto 0);
   variable op : std_logic_vector(OPERATION_WIDTH-1 downto 0);
function get_i (
      constant hi : integer;
      constant low : integer)
      return integer is
    begin
        return to_integer(unsigned(instruction(hi downto low)));
    end function get_i;

    function get_s (
      constant hi : integer;
      constant low : integer)
      return std_logic_vector is
    begin
        return instruction(hi downto low);
    end function get_s;

    function get_u (
      constant hi : integer;
      constant low : integer)
      return unsigned is
    begin
        return unsigned(instruction(hi downto low));
    end function get_u;
begin
      oLEDG <= (others => '0');
      -- default assignments
      registers_next <= registers_reg;
      pc_next <= pc_reg;
started_next <= started_reg;
counter_next <= counter_reg;
if_next <= if_reg;
-- Decode the operations
instruction := instructions(pc_reg);
op := instruction(OPERATION_WIDTH-1 downto 0);
-- Interpret operations
case op is
when asip_jump_if_true => 
oLEDG(3) <= '1';
if if_reg= '0' then
pc_next <= pc_reg + 1;
else
pc_next <= get_i(13, 4);
end if;

when asip_wait => 
oLEDG(1) <= '1';
-- allow to wait for one clock cycle
if get_i(35, 4) = 1 then
pc_next <= pc_reg + 1;
elsif started_reg = '0' then
counter_next <= get_i(35, 4);
started_next      <= '1';
else                            -- count down
counter_next <= counter_reg - 1;
if counter_reg = 0 then
started_next <= '0';
pc_next <= pc_reg + 1;
end if;
end if;

when asip_eq_rv => 
oLEDG(5) <= '1';
if registers_reg(get_i(7, 4)) = get_s(39, 8) then
if_next <= '1';
else
if_next <= '0';
end if;
pc_next <= pc_reg + 1;

when asip_jump => 
oLEDG(2) <= '1';
pc_next <= get_i(13, 4);

when asip_set_rv => 
oLEDG(0) <= '1';
registers_next(get_i(7, 4)) <= get_s(39, 8);
pc_next <= pc_reg + 1;

when asip_halt => 
oLEDG(6) <= '1';
pc_next <= pc_reg;

when asip_add_rvr => 
oLEDG(4) <= '1';
registers_next(get_i(43, 40)) <= std_logic_vector(to_unsigned(get_i(39, 8) + to_integer(unsigned(registers_reg(get_i(43, 40)))), 32));
pc_next <= pc_reg + 1;

when others => null;
end case;
end process;
-- connect first register to the LEDs
oLEDR <= registers_reg(0)(17 downto 0);
end architecture arch;
