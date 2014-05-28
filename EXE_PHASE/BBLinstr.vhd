library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity BBL is

	port
	(
		clk, reset : in std_logic;
		BBL_flag : in std_logic;
		IRin : in std_logic_vector(31 downto 0);
		CSR : in std_logic_vector(31 downto 0);
		PCnew : in std_logic_vector(31 downto 0);
		
		cond : out std_logic;
		ADDRout : out std_logic_vector(31 downto 0)

	);
	
end BBL;


architecture behavioral of BBL is


begin

	process(reset, clk) is 
	
	variable conv : std_logic_vector(31 downto 0);
	variable address : std_logic_vector(31 downto 0);
	
	begin																				-- CSR 	31	30	29	28
																						--			N	Z	C	V
		if (IRin(28 downto 27) = "00" and CSR(30) = '1') or
			(IRin(28 downto 27) = "01" and (((CSR(31) xor CSR(28)) or CSR(30)) = '0')) or
			(IRin(28 downto 27) = "10" and (CSR(29) = '0' or CSR(30) = '0')) or
			(IRin(28 downto 27) = "11") then
			
			if (BBL_flag = '1') then	
				conv := std_logic_vector(resize(unsigned(IRin(25 downto 0)), 32));

				address := PCnew + conv;
				ADDRout <= address;
			else
				ADDRout <= (others => '0');
			end if;
			cond <= '1';
		else
			cond <= '0';
		end if;
	end process; 


end behavioral;
