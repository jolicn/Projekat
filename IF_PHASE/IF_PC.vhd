library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_SIGNED.all;

entity IF_PC is

	port
	(
		CLK, reset	: in  std_logic;
		
		brnc_cond_in : in std_logic;
		JMPaddr : in std_logic_vector(31 downto 0);
		BBL_flag_in : in std_logic;
		
		PCinit	: in  std_logic_vector(31 downto 0);
		INIT : in std_logic;

		PC	: out std_logic_vector(31 downto 0);
		PCnew: out std_logic_vector(31 downto 0)
	);
end entity;

architecture Behavioral of IF_PC is

	signal PCtmp : std_logic_vector(31 downto 0);

begin

	process (CLK, reset, PCtmp, PCinit)
	
		variable PC_var : std_logic_vector(31 downto 0);
	
		begin
			PC_var := PCtmp;
			if (brnc_cond_in = '1' and BBL_flag_in = '1') then
				PC <= PC_var;
				PCnew <= JMPaddr;
			else
				PC <= PC_var;
				PCnew <= PC_var+1;
			end if;
			
			if (reset = '1' and INIT = '0') then
				PCtmp <= x"00000000";
			elsif (CLK'event and CLK = '1') then
				if (INIT = '1') then
					PCtmp <= PCinit;
				end if;
			end if;
		
		end process;

end architecture;
