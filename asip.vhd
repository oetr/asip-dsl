library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.instructions_lib.all;
----------------------------------------------------------------------
entity asip is
  port (
    iCLK_50 : in  std_logic;
    iKEY    : in  std_logic_vector(3 downto 0);
    oLEDR   : out std_logic_vector (17 downto 0);
    oLEDG   : out std_logic_vector (7 downto 0)
    );
end entity asip;
----------------------------------------------------------------------
architecture arch of asip is
  -- IO signals
  signal clk, reset     : std_logic;
  -- declare: 16 32-bit registers
  signal registers_reg  : regs_type;
  signal registers_next : regs_type;
  -- 1 if register
  signal if_reg         : register_type;
  signal if_next        : register_type;

  -- instruction counter
  signal i_reg, i_next : integer range 0 to INSTRUCTION_COUNT-1;

  -- wait counter
  signal wait_counter_reg  : unsigned(REG_WIDTH + REG_NUM_BITS -1 downto 0);
  signal wait_counter_next : unsigned(REG_WIDTH + REG_NUM_BITS -1 downto 0);

  signal started_reg, started_next : boolean;
  
begin
  --------------------------------------------------------------------
  -- Clock event update and default assignments
  --------------------------------------------------------------------
  process (clk, reset) is
  begin
    if reset = '1' then
      registers_reg    <= (others => (others => '0'));
      if_reg           <= (others => '0');
      i_reg            <= 0;
      wait_counter_reg <= (others => '0');
      started_reg      <= false;
    elsif rising_edge(clk) then
      registers_reg    <= registers_next;
      if_reg           <= if_next;
      i_reg            <= i_next;
      wait_counter_reg <= wait_counter_next;
      started_reg      <= started_next;
    end if;
  end process;

  --------------------------------------------------------------------
  -- working through the instructions
  --------------------------------------------------------------------
  process (i_reg, if_reg, registers_reg, started_reg,
           wait_counter_reg) is
    variable instruction      : std_logic_vector(INSTR_WIDTH-1 downto 0);
    variable instruction_name : std_logic_vector(INSTR_NAME_WIDTH-1 downto 0);
    variable reg              : integer range 0 to REG_NUM-1;
    variable val              : std_logic_vector(REG_WIDTH-1 downto 0);
    variable wait_cycles : std_logic_vector(INSTR_WIDTH - INSTR_NAME_WIDTH - 1
                                            downto 0);
  begin
    -- default assignments
    registers_next    <= registers_reg;
    if_next           <= if_reg;
    i_next            <= i_reg;
    wait_counter_next <= wait_counter_reg;
    started_next      <= started_reg;

    -- decode instruction
    instruction      := instructions(i_reg);
    instruction_name := instruction(INSTR_NAME_WIDTH-1 downto 0);
    val := instruction(INSTR_WIDTH-1 downto
                       INSTR_NAME_WIDTH + REG_NUM_BITS);
    reg := to_integer(unsigned(instruction
                               (INSTR_NAME_WIDTH + REG_NUM_BITS-1 downto
                                INSTR_NAME_WIDTH)));
    wait_cycles := instruction(INSTR_WIDTH - 1 downto INSTR_NAME_WIDTH);

    -- debugging
    oLEDG(7 downto 1) <= (others => '0');
    -- interpret instruction    
    case instruction_name is

      ----------------------------------------------------------------
      when "0000" =>                    -- set reg, val
        -- set the register
        registers_next(reg) <= val;
        i_next              <= i_reg + 1;

      --------------------------------------------------------------
      when "0001" =>                    -- wait(cycles)
        -- allow to wait for one clock cycle
        if wait_cycles = std_logic_vector(to_unsigned(1, wait_cycles'length)) then
          i_next <= i_reg + 1;
        elsif (not started_reg) then
          wait_counter_next <= unsigned(wait_cycles);
          started_next      <= true;
        else                            -- count down
          wait_counter_next <= wait_counter_reg - 1;
          if wait_counter_reg = 0 then
            started_next <= false;
            i_next       <= i_reg + 1;
          end if;
        end if;

        -- Debugging
        oLEDG(1) <= '1';

      ----------------------------------------------------------------
      when "0010" =>                    -- jmp, line
        i_next <= to_integer(unsigned(wait_cycles));

      ----------------------------------------------------------------
      when "0101" =>                    -- r=v?
        i_next <= i_reg + 1;
        if registers_reg(reg) = val then
          if_next(0) <= '1';
        else
          if_next <= (others => '0');
        end if;


      ----------------------------------------------------------------
      when "1100" =>                   -- jump if true
        if if_reg(0) = '1' then
          i_next <= to_integer(unsigned(wait_cycles));
        else
          i_next <= i_reg + 1;
        end if;

      ----------------------------------------------------------------
      when X"b" =>                      -- (+ reg val)
        oLEDG(3) <= '1';
        registers_next(reg) <= std_logic_vector(unsigned(registers_reg(reg)) +
                                                unsigned(val));
        i_next <= i_reg + 1;


      ----------------------------------------------------------------
      when "1111" =>                    -- halt(cycles)
        oLEDG(2) <= '1';

      ----------------------------------------------------------------
      when others => null;
    end case;
  end process;


  --------------------------------------------------------------------
  -- Input
  --------------------------------------------------------------------
  clk      <= iCLK_50;
  reset    <= not iKEY(0);
  oLEDG(0) <= reset;

  --------------------------------------------------------------------
  -- Output
  --------------------------------------------------------------------
  -- connect first register to the LEDs
  oLEDR <= registers_reg(0)(17 downto 0);
  
end architecture arch;
