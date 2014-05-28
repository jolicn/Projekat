library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use ieee.std_logic_arith.all;

use work.Pack.all;

entity ALU is

	port
	(
--		a, b : in std_logic_vector(31 downto 0);
		s : in std_logic;
		f : in std_logic;
		cin, vin : in std_logic;
		IRin : in std_logic_vector(31 downto 0);
		Rn, Rm, Rs, Rd, Imm : in std_logic_vector(31 downto 0);
		
		DP_I_in, DP_R_in, DP_flag : in std_logic;
		instr_type : in instruction_type;
		
		swap : out std_logic;
		dest_PC : out std_logic;
		swapAtoB, swapBtoA : out std_logic_vector(31 downto 0);
		addrAtoB, addrBtoA : out std_logic_vector(3 downto 0);
		cout, vout : out std_logic;
		nout, zout : out std_logic;
		output : out std_logic_vector(31 downto 0)
	);
end ALU;


architecture behavioral of ALU is

	component ADD

		port
		(
			a, b : in std_logic_vector(31 downto 0);
			cin : in std_logic;
			
			sum : out std_logic_vector(31 downto 0);
			cout : out std_logic;
			vout : out std_logic
			
		);

	end component;
	
	component SUB 
	
		port
		(
			a, b : in std_logic_vector(31 downto 0);
			cin : in std_logic;
			
			sum : out std_logic_vector(31 downto 0);
			cout : out std_logic;
			vout : out std_logic
			
		);
	
	end component;
	
	signal a, b : std_logic_vector(31 downto 0);
	
	signal ADDout, ADCout : std_logic_vector(31 downto 0);
	signal ADDcout, ADDvout, ADCcout, ADCvout : std_logic;
	
	signal SUBout, SBCout : std_logic_vector(31 downto 0);
	signal SUBcout, SUBvout, SBCcout, SBCvout : std_logic;

begin
	
	ADDd : component ADD 
		
		port map (
			
			a => a, b => b,
			cin => '0',
			
			sum => ADDout,
			cout => ADDcout,
			vout => ADDvout
			
		);
	
	ADC : component ADD
	
		port map (
		
			a => a, b => b,
			cin => cin,
			
			sum => ADCout,
			cout => ADCcout,
			vout => ADCvout		
		
		);
		
	SUBb : component SUB 
	
		port map (
		
			a => a, b => b,
			cin => '0',
			
			sum => SUBout,
			cout => SUBcout,
			vout => SUBvout			
		
		);	
	
	SBC : component SUB 
	
		port map (
		
			a => a, b => b,
			cin => cin,
			
			sum => SBCout,
			cout => SBCcout,
			vout => SBCvout			
		
		);	
		
	process(Rn, Rm, Rs, Imm, s, cin, vin, IRin, instr_type, ADDout, ADCout, 
		ADDcout, ADDvout, ADCcout, ADCvout, SUBout, SBCout, 
		SUBcout, SUBvout, SBCcout, SBCvout)
		
	variable result : std_logic_vector(31 downto 0);
	variable temp : integer range 0 to 31;
	
	begin 
		if (DP_flag = '1') then
		
--			a <= Rn;

			if (DP_R_in = '1') then
				b <= Rm;
				temp := to_integer(signed(Rs));
				if (IRin(6 downto 5) = "00") then					
					if (f = '1') then	
						a <= 	std_logic_vector(unsigned(Rn) rol temp);
					elsif (f = '0') then
						a <= std_logic_vector(unsigned(Rn) sll temp);
					end if;
				elsif (IRin(6 downto 5) = "01") then
					if (f = '1') then
						a <= 	std_logic_vector(unsigned(Rn) ror temp);
					elsif (f = '0') then
						a <= std_logic_vector(unsigned(Rn) srl temp);
					end if;
				elsif (IRin(6 downto 5) = "10") then
					if (f = '1') then
						a <= 	std_logic_vector(unsigned(Rn) ror temp);
					elsif (f = '0') then
						a <= to_stdlogicvector(to_bitvector(Rn) sra temp);
					end if;
				elsif (IRin(6 downto 5) = "11") then
					a <= Rn;
				end if;
			elsif (DP_I_in = '1') then
				b <= Imm;
			end if;
			
			result := x"00000000";
			
			if (IRin(20 downto 17) = "1111") then
				dest_PC <= '1';
			else
				dest_PC <= '0';
			end if;
			
			if(IRin(28 downto 25) = "0000") then
				result := a and b;
				cout <= cin;
				vout <= vin;
				
			elsif (IRin(28 downto 25) = "0010") then
				result := SUBout;
				cout <= SUBcout;
				vout <= SUBvout;
								
			elsif (IRin(28 downto 25) = "0100") then
				result := ADDout;
				cout <= ADDcout;
				vout <= ADDvout;
				
			elsif (IRin(28 downto 25) = "0101") then
				result := ADCout;
				cout <= ADCcout;
				vout <= ADCvout;
				
			elsif (IRin(28 downto 25) = "0110") then
				result := SBCout;
				cout <= SBCcout;
				vout <= SBCvout;
				
			elsif (IRin(28 downto 25) = "1000") then
				swapAtoB <= A;
				addrAtoB <= IRin(15 downto 12);
				swapBtoA <= B;
				addrBtoA <= IRin(24 downto 21);
				
				cout <= cin;
				vout <= vin;
				
	--		elsif (IRin(28 downto 25) = "1010") then		šta treba da se radi u CMP?
				
			elsif (IRin(28 downto 25) = "1101") then
				result := b;
				cout <= cin;
				vout <= vin;
				
			elsif (IRin(28 downto 25) = "1111") then
				result := b;
				cout <= cin;
				vout <= vin;
				
			end if;
			
			if (result = x"00000000") then zout <= '1'; else  zout <= '0';
			end if;
			if (result(31) = '1' and s = '1') then nout <= '1'; else nout <= '0' ;
			output <= result;
			end if;
			
		end if;
	end process; 
	
	swap <= '1' when IRin(28 downto 25) = "1000" else
				'0';

end behavioral;

