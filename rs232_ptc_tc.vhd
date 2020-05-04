LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
--PACKAGE arraypkg IS
--	TYPE arraybyte IS ARRAY(NATURAL RANGE <>) OF std_logic_vector(7 DOWNTO 0);
--END PACKAGE;

USE work.arraypkg.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
LIBRARY LPM;
USE LPM.lpm_components.ALL;

ENTITY rs232_ptc_tc IS
	PORT (
		nreset, clock : IN std_ulogic;
		rxd : IN std_logic;

		txframetrig : IN std_logic;
		-- txcount : in std_logic_vector(5 downto 0);
		txbuffer : IN arraybyte(31 DOWNTO 0);
		txd : OUT std_logic;
		rxframetrig : BUFFER std_logic;
		-- rxcount : OUT INTEGER RANGE 0 TO 7;
		rxbuffer : BUFFER arraybyte(31 DOWNTO 0);
		txend : buffer std_logic

	);
END rs232_ptc_tc;

ARCHITECTURE RTL OF rs232_ptc_tc IS

	COMPONENT rs232_phy_tc IS
		PORT (
			nreset, rxd, tx_byte_trig, clock : IN std_logic;
			tx_data : IN std_logic_vector(7 DOWNTO 0);
			tx_end : OUT std_logic;
			txd, rx_byte_trig : OUT std_logic;
			rx_data : OUT std_logic_vector(7 DOWNTO 0)
		);
	END COMPONENT;

	SIGNAL rxbufferq, txbufferq : arraybyte(31 DOWNTO 0);
	SIGNAL lastbyte : std_logic_vector(7 DOWNTO 0);
	SIGNAL txcount : INTEGER RANGE 0 TO 7;

	SIGNAL rxframetrigq : std_logic;
	SIGNAL rxbytetrig, txbytetrig : std_logic;
	SIGNAL pulse, clrcount : std_logic;
	SIGNAL rxdataq, txdataq : std_logic_vector(7 DOWNTO 0);
	SIGNAL txbyteend : std_logic;
BEGIN
	----------------------------------------------------------------------------------------------------
	-- transmittion
	----------------------------------------------------------------------------------------------------
	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			txbytetrig <= '0';
		ELSIF rising_edge(clock) THEN
			txbytetrig <= txframetrig OR (txbyteend AND (NOT txend));
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			txbufferq <= (txbufferq'RANGE => X"00");
			lastbyte <= X"00";
			txcount <= 0;
			txend <= '1';
		ELSIF rising_edge(clock) THEN
			IF txframetrig = '1' THEN
				txbufferq <= txbuffer;
				lastbyte <= X"00";
				txcount <= 0;
				txend <= '0';
			ELSIF txbyteend = '1' AND txend = '0' THEN
				txbufferq <= txbufferq(30 DOWNTO 0) & X"00";
				lastbyte <= txbufferq(31);
				IF txcount = 31 OR (lastbyte = X"0A" AND txbufferq(31) = X"0D") THEN
					txend <= '1';
				END IF;
				IF txcount /= 31 THEN
					txcount <= txcount + 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	txdataq <= txbufferq(31);

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			rxframetrigq <= '0';
			rxbufferq <= (rxbufferq'RANGE => X"00");
			-- rxcount <= 0;
		ELSIF rising_edge(clock) THEN
			IF rxbufferq(1) = X"0A" AND rxbufferq(0) = X"0D" THEN
				rxframetrigq <= '1';
			ELSE
				rxframetrigq <= '0';
			END IF;
			IF clrcount = '1' OR rxframetrigq = '1' THEN
				rxbufferq <= (rxbufferq'RANGE => X"00");
				-- rxcount <= 0;
			ELSIF rxbytetrig = '1' THEN
				rxbufferq <= rxbufferq(30 DOWNTO 0) & rxdataq;
				-- IF rxcount /= 31 THEN
				-- rxcount <= rxcount + 1;
				-- END IF;
			END IF;
		END IF;
	END PROCESS;

	-- trim the lead space
	PROCESS (nreset, clock)
		-- variable bitspace : std_logic;
		VARIABLE rxbuffertmp : arraybyte(31 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			rxbuffer <= (rxbuffer'RANGE => X"00");
			rxbuffertmp := (rxbuffertmp'RANGE => X"00");
			-- bitspace <= '0';
		ELSIF rising_edge(clock) THEN
			IF rxframetrigq = '1' THEN
				rxbuffer <= (rxbuffer'RANGE => X"00");
				rxbuffertmp := rxbufferq;
				-- bitspace := '1';
			ELSE
				IF (rxbuffertmp(31) /= X"00" AND rxbuffertmp(31) /= X"20") THEN
					rxbuffer <= rxbuffertmp;
				END IF;
				IF (rxbuffertmp(31) = X"00" OR rxbuffertmp(31) = X"20") THEN
					rxbuffertmp := rxbuffertmp(30 DOWNTO 0) & X"00";
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
	BEGIN
		IF nreset = '0' THEN
			c := 31;
			rxframetrig <= '0';
		ELSIF rising_edge(clock) THEN
			IF rxframetrigq = '1' THEN
				rxframetrig <= '0';
				c := 0;
			ELSE
				IF c = 30 THEN
					rxframetrig <= '1';
				ELSE
					rxframetrig <= '0';
				END IF;
				IF c /= 31 THEN
					c := c + 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	-- 100000000*0.00001 = 1000
	PROCESS (nreset, clock, rxbytetrig)
		VARIABLE c : INTEGER RANGE 0 TO 2047;
	BEGIN
		IF nreset = '0' OR rxbytetrig = '1' THEN
			c := 0;
			pulse <= '0';
		ELSIF rising_edge(clock) THEN
			IF c = 500 THEN
				pulse <= '1';
				c := 0;
			ELSE
				pulse <= '0';
				c := c + 1;
			END IF;
		END IF;
	END PROCESS;

	-- 10us*1000 = 10ms
	PROCESS (nreset, clock, rxbytetrig)
		VARIABLE c : INTEGER RANGE 0 TO 1023;
	BEGIN
		IF nreset = '0' OR rxbytetrig = '1' THEN
			c := 0;
			clrcount <= '0';
		ELSIF rising_edge(clock) THEN
			IF pulse = '1' THEN
				IF c = 999 THEN
					clrcount <= '1';
				ELSE
					clrcount <= '0';
				END IF;
				IF c /= 1023 THEN
					c := c + 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	rs232_0 : rs232_phy_tc PORT MAP
	(
		nreset => nreset,
		clock => clock,
		rxd => rxd,
		tx_byte_trig => txbytetrig,
		tx_data => txdataq,
		tx_end => txbyteend,
		txd => txd,
		rx_byte_trig => rxbytetrig,
		rx_data => rxdataq
	);

END RTL;