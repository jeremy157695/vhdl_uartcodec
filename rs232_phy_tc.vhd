LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;
LIBRARY LPM;
USE LPM.lpm_components.ALL;

ENTITY rs232_phy_tc IS
	PORT (
		nreset, rxd, tx_byte_trig, clock : IN std_logic;
		tx_data : IN std_logic_vector(7 DOWNTO 0);
		tx_end : OUT std_logic;
		txd, rx_byte_trig : OUT std_logic;
		rx_data : OUT std_logic_vector(7 DOWNTO 0)
	);
END rs232_phy_tc;

ARCHITECTURE RTL OF rs232_phy_tc IS
	CONSTANT duty : INTEGER := 434 - 1; -- 50M/115200 = 434
	CONSTANT half_duty : INTEGER := 217 - 1;
	CONSTANT duty_tx : INTEGER := 434 - 1;
	CONSTANT half_duty_tx : INTEGER := 217 - 1;
	SIGNAL latch, start, stop : std_logic;
	SIGNAL rxclk, txclk : std_logic;
	SIGNAL rxdataq, txdataq : std_logic_vector(9 DOWNTO 0);

BEGIN

	PROCESS (nreset, clock, stop)
		VARIABLE c : INTEGER RANGE 0 TO 8191;
	BEGIN
		IF nreset = '0' THEN
			start <= '0';
			c := 0;
		ELSIF rising_edge(clock) THEN
			IF stop = '1' THEN
				start <= '0';
			ELSIF c = half_duty THEN
				start <= '1';
			END IF;
			IF stop = '1' OR (start = '0' AND rxd = '1') THEN
				c := 0;
			ELSIF rxd = '0' AND c /= 8191 THEN
				c := c + 1;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock, start)
		VARIABLE c : INTEGER RANGE 0 TO 8191;
	BEGIN
		IF nreset = '0' OR start = '0' THEN
			rxclk <= '0';
			c := 0;
		ELSIF rising_edge(clock) THEN
			IF c = duty THEN
				rxclk <= '1';
			ELSE
				rxclk <= '0';
			END IF;
			IF c = duty THEN
				c := 0;
			ELSE
				c := c + 1;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			rxdataq <= "0000000000";
		ELSIF rising_edge(clock) THEN
			IF rxclk = '1' THEN
				rxdataq <= rxd & rxdataq(9 DOWNTO 1);
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 3;
	BEGIN
		IF nreset = '0' THEN
			c := 3;
			latch <= '0';
		ELSIF rising_edge(clock) THEN
			IF c = 1 THEN
				latch <= '1';
			ELSE
				latch <= '0';
			END IF;
			IF start = '1' THEN
				c := 0;
			ELSIF c /= 3 THEN
				c := c + 1;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			rx_data <= (OTHERS => '0');
			rx_byte_trig <= '0';
		ELSIF rising_edge(clock) THEN
			IF Latch = '1' THEN
				rx_data <= rxdataq(8 DOWNTO 1);
			END IF;
			rx_byte_trig <= latch;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock, start)
		VARIABLE c : INTEGER RANGE 0 TO 15;
	BEGIN
		IF nreset = '0' OR start = '0' THEN
			stop <= '0';
			c := 0;
		ELSIF rising_edge(clock) THEN
			IF rxclk = '1' THEN
				IF c = 8 THEN
					stop <= '1';
				END IF;
				IF c < 15 THEN
					c := c + 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	---------------------------------------------------------------------------------------------------
	-- transmitter
	---------------------------------------------------------------------------------------------------
	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 8191;
	BEGIN
		IF nreset = '0' THEN
			txclk <= '0';
			c := 0;
		ELSIF rising_edge(clock) THEN
			IF tx_byte_trig = '1' THEN
				txclk <= '0';
				c := 0;
			ELSE
				IF c = 5 THEN
					txclk <= '1';
				ELSE
					txclk <= '0';
				END IF;
				IF c = duty_tx THEN
					c := 0;
				ELSE
					c := c + 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 15;
	BEGIN
		IF nreset = '0' THEN
			txdataq <= (OTHERS => '1');
			txd <= '1';
			tx_end <= '0';
			c := 15;
		ELSIF rising_edge(clock) THEN
			IF txclk = '1' AND c = 10 THEN
				tx_end <= '1';
			ELSE
				tx_end <= '0';
			END IF;
			IF tx_byte_trig = '1' THEN
				txdataq <= '1' & tx_data & '0';
				c := 0;
				txd <= '1';
			ELSIF txclk = '1' THEN
				--			IF c = 10 THEN
				--				tx_end <= '1';
				--			END IF;
				IF c /= 15 THEN
					c := c + 1;
				END IF;
				txd <= txdataq(0);
				txdataq(0) <= txdataq(1);
				txdataq(1) <= txdataq(2);
				txdataq(2) <= txdataq(3);
				txdataq(3) <= txdataq(4);
				txdataq(4) <= txdataq(5);
				txdataq(5) <= txdataq(6);
				txdataq(6) <= txdataq(7);
				txdataq(7) <= txdataq(8);
				txdataq(8) <= txdataq(9);
			END IF;
		END IF;
	END PROCESS;

END RTL;