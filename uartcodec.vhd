LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

PACKAGE arraypkg IS
	TYPE arraybyte IS ARRAY(NATURAL RANGE <>) OF std_logic_vector(7 DOWNTO 0);
	TYPE array10 IS ARRAY(NATURAL RANGE <>) OF std_logic_vector(9 DOWNTO 0);
END PACKAGE;

USE work.arraypkg.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;
--USE IEEE.numeric_std.ALL;

--LIBRARY LPM;
--USE LPM.lpm_components.ALL;

ENTITY uartcodec IS
	PORT (
		nreset, clock : IN std_ulogic;
		rxd : IN std_logic;

		floatsw : IN std_logic;
		temperature : IN arraybyte(1 DOWNTO 0);
		humidity : IN arraybyte(1 DOWNTO 0);
		voltdc : IN array10(3 DOWNTO 0);

		txd : OUT std_logic;
		relaypwr : BUFFER std_logic_vector(7 DOWNTO 0);
		wledon : BUFFER std_logic;
		briofrgb0, briofrgb1 : BUFFER arraybyte(2 DOWNTO 0);
		rgbmode0, rgbmode1 : BUFFER std_logic_vector(7 DOWNTO 0)

	);
END uartcodec;

ARCHITECTURE RTL OF uartcodec IS

	COMPONENT rs232_ptc_tc IS
		PORT (
			nreset, clock : IN std_ulogic;
			rxd : IN std_logic;

			txframetrig : IN std_logic;
			txbuffer : IN arraybyte(31 DOWNTO 0);
			txd : OUT std_logic;
			rxframetrig : BUFFER std_logic;
			rxbuffer : BUFFER arraybyte(31 DOWNTO 0);
			txend : OUT std_logic

		);
	END COMPONENT;

	FUNCTION char_to_std_logic_vector(c : CHARACTER) RETURN std_logic_vector IS
		VARIABLE slv : std_logic_vector(7 DOWNTO 0);
	BEGIN
		CASE c IS
			WHEN '-' => slv := X"2D";
			WHEN '0' => slv := X"30";
			WHEN '1' => slv := X"31";
			WHEN '2' => slv := X"32";
			WHEN '3' => slv := X"33";
			WHEN '4' => slv := X"34";
			WHEN '5' => slv := X"35";
			WHEN '6' => slv := X"36";
			WHEN '7' => slv := X"37";
			WHEN '8' => slv := X"38";
			WHEN '9' => slv := X"39";
			WHEN 'a' | 'A' => slv := X"41";
			WHEN 'b' | 'B' => slv := X"42";
			WHEN 'c' | 'C' => slv := X"43";
			WHEN 'd' | 'D' => slv := X"44";
			WHEN 'e' | 'E' => slv := X"45";
			WHEN 'f' | 'F' => slv := X"46";
			WHEN 'g' | 'G' => slv := X"47";
			WHEN 'h' | 'H' => slv := X"48";
			WHEN 'i' | 'I' => slv := X"49";
			WHEN 'j' | 'J' => slv := X"4A";
			WHEN 'k' | 'K' => slv := X"4B";
			WHEN 'l' | 'L' => slv := X"4C";
			WHEN 'm' | 'M' => slv := X"4D";
			WHEN 'n' | 'N' => slv := X"4E";
			WHEN 'o' | 'O' => slv := X"4F";
			WHEN 'p' | 'P' => slv := X"50";
			WHEN 'q' | 'Q' => slv := X"51";
			WHEN 'r' | 'R' => slv := X"52";
			WHEN 's' | 'S' => slv := X"53";
			WHEN 't' | 'T' => slv := X"54";
			WHEN 'u' | 'U' => slv := X"55";
			WHEN 'v' | 'V' => slv := X"56";
			WHEN 'w' | 'W' => slv := X"57";
			WHEN 'x' | 'X' => slv := X"58";
			WHEN 'y' | 'Y' => slv := X"59";
			WHEN 'z' | 'Z' => slv := X"5A";
			WHEN OTHERS => slv := X"20";
		END CASE;
		RETURN slv;
	END char_to_std_logic_vector;

	FUNCTION string_to_std_logic_vector(s : STRING) RETURN arraybyte IS
		VARIABLE slv : arraybyte(s'high - 1 DOWNTO 0);
		--		VARIABLE k : INTEGER;
	BEGIN
		--		k := s'high - s'low;
		FOR i IN s'low TO s'high LOOP
			slv(s'high - i) := char_to_std_logic_vector(s(i));
			--			k := k - 1;
		END LOOP;
		RETURN slv;
	END string_to_std_logic_vector;

	FUNCTION ascii_to_hex(a : arraybyte) RETURN std_logic_vector IS
		VARIABLE slv, slv100, slv10 : std_logic_vector(15 DOWNTO 0);
		VARIABLE slv1 : std_logic_vector(7 DOWNTO 0);
	BEGIN
		slv1 := a(0) - X"30";
		slv10 := (a(1) - X"30") * conv_std_logic_vector(10, 8);
		slv100 := (a(2) - X"30") * conv_std_logic_vector(100, 8);
		slv := slv100 + slv10 + slv1;
		RETURN slv(7 DOWNTO 0);
	END ascii_to_hex;

	CONSTANT hexendcode : arraybyte(1 DOWNTO 0) := X"0A" & X"0D";
	CONSTANT strpwron : STRING := "POWERON--"; -- "pwron--0"
	CONSTANT strpwroff : STRING := "POWEROFF--"; -- "pwroff--0"
	CONSTANT strledon : STRING := "LEDON"; -- "ledon"
	CONSTANT strledoff : STRING := "LEDOFF"; -- "ledoff"
	CONSTANT strrgbmd0 : STRING := "RGBMD--0"; -- "rgbmd--0 0"	
	CONSTANT strrgbmd1 : STRING := "RGBMD--1"; -- "rgbmd--1 0"
	CONSTANT strrgbval0 : STRING := "RGBVAL--0"; -- "rgbval--0 255 255 0"	
	CONSTANT strrgbval1 : STRING := "RGBVAL--1"; -- "rgbval--1 255 255 0"

	CONSTANT strgetpwr : STRING := "GETPOWER"; -- "getpwr"
	CONSTANT strgetled : STRING := "GETLED"; -- "getled"
	CONSTANT strgettemp0 : STRING := "GETTEMP--0";	-- "gettemp--0"; "gettemp--1"
	CONSTANT strgettemp1 : STRING := "GETTEMP--1";	-- "gettemp--0"; "gettemp--1"
	CONSTANT strgetrgbmd0 : STRING := "GETRGBMD--0"; -- "getrgbmd--0"
	CONSTANT strgetrgbmd1 : STRING := "GETRGBMD--1"; -- "getrgbmd--1"
	CONSTANT strgetrgbval0 : STRING := "GETRGBVAL--0"; -- "getrgbmd--0"
	CONSTANT strgetrgbval1 : STRING := "GETRGBVAL--1"; -- "getrgbmd--1"

	CONSTANT strgetdc : STRING := "GETDC"; -- "getdc"
	CONSTANT strgetfs : STRING := "GETFS"; -- "getfs"

	SIGNAL slvtemp0, slvtemp1 : std_logic_vector(7 DOWNTO 0);
	SIGNAL slvhumidity0, slvhumidity1 : std_logic_vector(7 DOWNTO 0);

	SIGNAL strtemp0, strtemp1 : arraybyte(2 DOWNTO 0);
	SIGNAL strhumidity0, strhumidity1 : arraybyte(1 DOWNTO 0);

	SIGNAL strdc0, strdc1, strdc2, strdc3 : arraybyte(4 DOWNTO 0);

	SIGNAL rtcode : INTEGER RANGE 0 TO 15;

	SIGNAL rxframetrig : std_logic;
	SIGNAL rxdata : arraybyte(31 DOWNTO 0);

	SIGNAL asciir0, asciig0, asciib0 : arraybyte(2 DOWNTO 0);
	SIGNAL asciir1, asciig1, asciib1 : arraybyte(2 DOWNTO 0);

	SIGNAL rttrig, txtrimtrig, txframetrig : std_logic;

	SIGNAL txdata, txdataq, txdatatmp : arraybyte(31 DOWNTO 0);
BEGIN

	-- temperature = read data * 165 / 256 - 40
	PROCESS (nreset, temperature, clock)
		VARIABLE t : std_logic_vector(15 DOWNTO 0);
	BEGIN
		t := temperature(0) * conv_std_logic_vector(165, 8);
		slvtemp0 <= t(15 DOWNTO 8) - 40;
	END PROCESS;

	PROCESS (nreset, temperature, clock)
		VARIABLE t : std_logic_vector(15 DOWNTO 0);
	BEGIN
		t := temperature(1) * conv_std_logic_vector(165, 8);
		slvtemp1 <= t(15 DOWNTO 8) - 40;
	END PROCESS;

	-- humidity = read data * 100 / 256 %
	PROCESS (nreset, humidity, clock)
		VARIABLE h : std_logic_vector(15 DOWNTO 0);
	BEGIN
		h := humidity(0) * conv_std_logic_vector(100, 8);
		slvhumidity0 <= h(15 DOWNTO 8);
	END PROCESS;

	PROCESS (nreset, humidity, clock)
		VARIABLE h : std_logic_vector(15 DOWNTO 0);
	BEGIN
		h := humidity(1) * conv_std_logic_vector(100, 8);
		slvhumidity1 <= h(15 DOWNTO 8);
	END PROCESS;

	---------------------------------------------------------------------------------------------------
	---------------------------------------------------------------------------------------------------
	-- convert temperature ascii to hex
	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(7 DOWNTO 0);
		VARIABLE asc : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				IF asc(2) = X"30" THEN
					IF asc(1) = X"30" THEN
						strtemp0(1) <= X"20";
					ELSE
						strtemp0(1) <= asc(1);
					END IF;
					strtemp0(2) <= X"20";
				ELSE
					strtemp0(2 DOWNTO 1) <= asc(2 DOWNTO 1);
				END IF;
				strtemp0(0) <= asc(0);
			END IF;
			IF c = 0 THEN
				slv := slvtemp0;
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 100 THEN
					slv := slv - 100;
					asc(2) := asc(2) + 1;
				ELSIF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(7 DOWNTO 0);
		VARIABLE asc : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				IF asc(2) = X"30" THEN
					IF asc(1) = X"30" THEN
						strtemp1(1) <= X"20";
					ELSE
						strtemp1(1) <= asc(1);
					END IF;
					strtemp1(2) <= X"20";
				ELSE
					strtemp1(2 DOWNTO 1) <= asc(2 DOWNTO 1);
				END IF;
				strtemp1(0) <= asc(0);
			END IF;
			IF c = 0 THEN
				slv := slvtemp1;
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 100 THEN
					slv := slv - 100;
					asc(2) := asc(2) + 1;
				ELSIF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	-- convert humidity ascii to hex 
	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(7 DOWNTO 0);
		VARIABLE asc : arraybyte(1 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				IF asc(1) = X"30" THEN
					strhumidity0(1) <= X"20";
				ELSE
					strhumidity0(1) <= asc(1);
				END IF;
				strhumidity0(0) <= asc(0);
			END IF;
			IF c = 0 THEN
				slv := slvhumidity0;
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(7 DOWNTO 0);
		VARIABLE asc : arraybyte(1 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				IF asc(1) = X"30" THEN
					strhumidity1(1) <= X"20";
				ELSE
					strhumidity1(1) <= asc(1);
				END IF;
				strhumidity1(0) <= asc(0);
			END IF;
			IF c = 0 THEN
				slv := slvhumidity1;
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	-- convert dc voltage ascii to hex
	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(9 DOWNTO 0);
		VARIABLE asc : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				strdc0 <= asc(2 DOWNTO 1) & X"2E" & asc(0) & X"56";
			END IF;
			IF c = 0 THEN
				slv := voltdc(0);
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 100 THEN
					slv := slv - 100;
					asc(2) := asc(2) + 1;
				ELSIF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(9 DOWNTO 0);
		VARIABLE asc : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				strdc1 <= asc(2 DOWNTO 1) & X"2E" & asc(0) & X"56";
			END IF;
			IF c = 0 THEN
				slv := voltdc(1);
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 100 THEN
					slv := slv - 100;
					asc(2) := asc(2) + 1;
				ELSIF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(9 DOWNTO 0);
		VARIABLE asc : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				strdc2 <= asc(2 DOWNTO 1) & X"2E" & asc(0) & X"56";
			END IF;
			IF c = 0 THEN
				slv := voltdc(2);
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 100 THEN
					slv := slv - 100;
					asc(2) := asc(2) + 1;
				ELSIF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 31;
		VARIABLE slv : std_logic_vector(9 DOWNTO 0);
		VARIABLE asc : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			slv := (slv'RANGE => '0');
			asc := (asc'RANGE => X"30");
		ELSIF rising_edge(clock) THEN
			IF c = 31 THEN
				strdc3 <= asc(2 DOWNTO 1) & X"2E" & asc(0) & X"56";
			END IF;
			IF c = 0 THEN
				slv := voltdc(3);
				asc := (asc'RANGE => X"30");
			ELSE
				IF slv >= 100 THEN
					slv := slv - 100;
					asc(2) := asc(2) + 1;
				ELSIF slv >= 10 THEN
					slv := slv - 10;
					asc(1) := asc(1) + 1;
				ELSIF slv >= 1 THEN
					slv := slv - 1;
					asc(0) := asc(0) + 1;
				END IF;
			END IF;
			c := c + 1;
		END IF;
	END PROCESS;

	---------------------------------------------------------------------------------------------------
	---------------------------------------------------------------------------------------------------

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			relaypwr <= (relaypwr'RANGE => '1');
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' THEN
				IF rxdata(rxdata'high DOWNTO (rxdata'high - strpwron'high + 1)) = string_to_std_logic_vector(strpwron) THEN
					relaypwr(conv_integer(rxdata(rxdata'high - strpwron'high) - X"30")) <= '1';
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strpwroff'high + 1)) = string_to_std_logic_vector(strpwroff) THEN
					relaypwr(conv_integer(rxdata(rxdata'high - strpwroff'high) - X"30")) <= '0';
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			wledon <= '1';
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' THEN
				IF rxdata(rxdata'high DOWNTO (rxdata'high - strledon'high + 1)) = string_to_std_logic_vector(strledon) THEN
					wledon <= '1';
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strledoff'high + 1)) = string_to_std_logic_vector(strledoff) THEN
					wledon <= '0';
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			rgbmode0 <= X"41";
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' THEN
				IF rxdata(rxdata'high DOWNTO (rxdata'high - strrgbmd0'high + 1)) = string_to_std_logic_vector(strrgbmd0) THEN
					rgbmode0 <= rxdata(rxdata'high - strrgbmd0'high - 1);
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			rgbmode1 <= X"41";
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' THEN
				IF rxdata(rxdata'high DOWNTO (rxdata'high - strrgbmd1'high + 1)) = string_to_std_logic_vector(strrgbmd1) THEN
					rgbmode1 <= rxdata(rxdata'high - strrgbmd1'high - 1);
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 3;
		VARIABLE c1 : INTEGER RANGE 0 TO 31;
		VARIABLE r0tmp, g0tmp, b0tmp : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			asciir0 <= string_to_std_logic_vector("255");
			asciig0 <= string_to_std_logic_vector("255");
			asciib0 <= string_to_std_logic_vector("255");
			r0tmp := string_to_std_logic_vector("255");
			g0tmp := string_to_std_logic_vector("255");
			b0tmp := string_to_std_logic_vector("255");
			c := 0;
			c1 := 0;
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' AND (rxdata(rxdata'high DOWNTO (rxdata'high - strrgbval0'high + 1)) = string_to_std_logic_vector(strrgbval0)) THEN
				r0tmp := string_to_std_logic_vector("000");
				g0tmp := string_to_std_logic_vector("000");
				b0tmp := string_to_std_logic_vector("000");
				c := 0;
				c1 := rxdata'high - strrgbval0'high - 1;
			ELSE
				IF (rxdata(c1) < X"30" OR rxdata(c1) > X"39") AND c /= 3 THEN
					c := c + 1;
				END IF;
				IF c1 = 1 THEN
					asciir0 <= r0tmp;
					asciig0 <= g0tmp;
					asciib0 <= b0tmp;
				END IF;
				IF c = 0 THEN
					IF rxdata(c1) /= X"20" THEN
						r0tmp := r0tmp(1 DOWNTO 0) & rxdata(c1);
					END IF;
				END IF;
				IF c = 1 THEN
					IF rxdata(c1) /= X"20" THEN
						g0tmp := g0tmp(1 DOWNTO 0) & rxdata(c1);
					END IF;
				END IF;
				IF c = 2 THEN
					IF rxdata(c1) /= X"20" THEN
						b0tmp := b0tmp(1 DOWNTO 0) & rxdata(c1);
					END IF;
				END IF;
				IF c1 /= 0 THEN
					c1 := c1 - 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	briofrgb0(2) <= ascii_to_hex(asciir0);
	briofrgb0(1) <= ascii_to_hex(asciig0);
	briofrgb0(0) <= ascii_to_hex(asciib0);

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 3;
		VARIABLE c1 : INTEGER RANGE 0 TO 31;
		VARIABLE r1tmp, g1tmp, b1tmp : arraybyte(2 DOWNTO 0);
	BEGIN
		IF nreset = '0' THEN
			asciir1 <= string_to_std_logic_vector("255");
			asciig1 <= string_to_std_logic_vector("255");
			asciib1 <= string_to_std_logic_vector("255");
			r1tmp := string_to_std_logic_vector("255");
			g1tmp := string_to_std_logic_vector("255");
			b1tmp := string_to_std_logic_vector("255");
			c := 0;
			c1 := 0;
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' AND (rxdata(rxdata'high DOWNTO (rxdata'high - strrgbval1'high + 1)) = string_to_std_logic_vector(strrgbval1)) THEN
				r1tmp := string_to_std_logic_vector("000");
				g1tmp := string_to_std_logic_vector("000");
				b1tmp := string_to_std_logic_vector("000");
				c := 0;
				c1 := rxdata'high - strrgbval1'high - 1;
			ELSE
				IF (rxdata(c1) < X"30" OR rxdata(c1) > X"39") AND c /= 3 THEN
					c := c + 1;
				END IF;
				IF c1 = 1 THEN
					asciir1 <= r1tmp;
					asciig1 <= g1tmp;
					asciib1 <= b1tmp;
				END IF;
				IF c = 0 THEN
					IF rxdata(c1) /= X"20" THEN
						r1tmp := r1tmp(1 DOWNTO 0) & rxdata(c1);
					END IF;
				END IF;
				IF c = 1 THEN
					IF rxdata(c1) /= X"20" THEN
						g1tmp := g1tmp(1 DOWNTO 0) & rxdata(c1);
					END IF;
				END IF;
				IF c = 2 THEN
					IF rxdata(c1) /= X"20" THEN
						b1tmp := b1tmp(1 DOWNTO 0) & rxdata(c1);
					END IF;
				END IF;
				IF c1 /= 0 THEN
					c1 := c1 - 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	briofrgb1(2) <= ascii_to_hex(asciir1);
	briofrgb1(1) <= ascii_to_hex(asciig1);
	briofrgb1(0) <= ascii_to_hex(asciib1);

	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 63;
	BEGIN
		IF nreset = '0' THEN
			c := 63;
			rttrig <= '0';
			txtrimtrig <= '0';
			txframetrig <= '0';
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' THEN
				c := 0;
				rttrig <= '0';
				txtrimtrig <= '0';
				txframetrig <= '0';
			ELSIF rtcode /= 0 THEN
				IF c = 3 THEN
					rttrig <= '1';
				ELSE
					rttrig <= '0';
				END IF;
				IF c = 5 THEN
					txtrimtrig <= '1';
				ELSE
					txtrimtrig <= '0';
				END IF;
				IF c = 40 THEN
					txframetrig <= '1';
				ELSE
					txframetrig <= '0';
				END IF;
				IF c /= 63 THEN
					c := c + 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			rtcode <= 0;
		ELSIF rising_edge(clock) THEN
			IF rxframetrig = '1' THEN
				IF rxdata(rxdata'high DOWNTO (rxdata'high - strgetpwr'high + 1)) = string_to_std_logic_vector(strgetpwr) THEN
					rtcode <= 1;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetled'high + 1)) = string_to_std_logic_vector(strgetled) THEN
					rtcode <= 2;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgettemp0'high + 1)) = string_to_std_logic_vector(strgettemp0) THEN
					rtcode <= 3;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgettemp1'high + 1)) = string_to_std_logic_vector(strgettemp1) THEN
					rtcode <= 4;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetrgbmd0'high + 1)) = string_to_std_logic_vector(strgetrgbmd0) THEN
					rtcode <= 5;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetrgbmd1'high + 1)) = string_to_std_logic_vector(strgetrgbmd1) THEN
					rtcode <= 6;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetrgbval0'high + 1)) = string_to_std_logic_vector(strgetrgbval0) THEN
					rtcode <= 7;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetrgbval1'high + 1)) = string_to_std_logic_vector(strgetrgbval1) THEN
					rtcode <= 8;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetdc'high + 1)) = string_to_std_logic_vector(strgetdc) THEN
					rtcode <= 9;
				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - strgetfs'high + 1)) = string_to_std_logic_vector(strgetfs) THEN
					rtcode <= 10;
					--				ELSIF rxdata(rxdata'high DOWNTO (rxdata'high - 1)) /= string_to_std_logic_vector("ID") THEN
					--					rtcode <= 11;
				ELSE
					rtcode <= 0;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	PROCESS (nreset, clock)
	BEGIN
		IF nreset = '0' THEN
			txdataq <= (txdataq'RANGE => X"20");
		ELSIF rising_edge(clock) THEN
			IF rttrig = '1' THEN
				txdataq(1 DOWNTO 0) <= hexendcode;
				CASE rtcode IS
					WHEN 1 => -- return power state
						txdataq(31 DOWNTO 17) <= string_to_std_logic_vector("power state is ");
						FOR i IN relaypwr'RANGE LOOP
							IF relaypwr(i) = '1' THEN
								txdataq(i + 8) <= X"31";
							ELSE
								txdataq(i + 8) <= X"30";
							END IF;
						END LOOP;
						txdataq(7 DOWNTO 2) <= (7 DOWNTO 2 => X"20");
					WHEN 2 => -- return wled state
						txdataq(31 DOWNTO 18) <= string_to_std_logic_vector("wled state is ");
						txdataq(17) <= X"30" + conv_std_logic_vector(wledon, 1);
						txdataq(16 DOWNTO 2) <= (16 DOWNTO 2 => X"20");
					WHEN 3 => -- return temperature sensor1 data
						txdataq(31 DOWNTO 16) <= string_to_std_logic_vector("sensor0 data is ");
						txdataq(15 DOWNTO 12) <= strtemp0 & X"20";
						txdataq(11 DOWNTO 9) <= strhumidity0 & X"25";
						txdataq(8 DOWNTO 2) <= (8 DOWNTO 2 => X"20");
					WHEN 4 => -- return temperature sensor2 data
						txdataq(31 DOWNTO 16) <= string_to_std_logic_vector("sensor1 data is ");
						txdataq(15 DOWNTO 12) <= strtemp1 & X"20";
						txdataq(11 DOWNTO 9) <= strhumidity1 & X"25";
						txdataq(8 DOWNTO 2) <= (8 DOWNTO 2 => X"20");
					WHEN 5 => -- return rgb0 mode
						txdataq(31 DOWNTO 19) <= string_to_std_logic_vector("rgb0 mode is ");
						txdataq(18 DOWNTO 17) <= rgbmode0 & X"20";
						txdataq(16 DOWNTO 2) <= (16 DOWNTO 2 => X"20");
					WHEN 6 => -- return rgb1 mode
						txdataq(31 DOWNTO 19) <= string_to_std_logic_vector("rgb1 mode is ");
						txdataq(18 DOWNTO 17) <= rgbmode1 & X"20";
						txdataq(16 DOWNTO 2) <= (16 DOWNTO 2 => X"20");
					WHEN 7 => -- return rgb0 data
						txdataq(31 DOWNTO 19) <= string_to_std_logic_vector("rgb0 data is ");
						txdataq(18 DOWNTO 15) <= asciir0 & X"20";
						txdataq(14 DOWNTO 11) <= asciig0 & X"20";
						txdataq(10 DOWNTO 7) <= asciib0 & X"20";
						txdataq(6 DOWNTO 2) <= (6 DOWNTO 2 => X"20");
					WHEN 8 => -- return rgb1 data
						txdataq(31 DOWNTO 19) <= string_to_std_logic_vector("rgb1 data is ");
						txdataq(18 DOWNTO 15) <= asciir1 & X"20";
						txdataq(14 DOWNTO 11) <= asciig1 & X"20";
						txdataq(10 DOWNTO 7) <= asciib1 & X"20";
						txdataq(6 DOWNTO 2) <= (6 DOWNTO 2 => X"20");
					WHEN 9 => -- return dc sensor data
						txdataq(31 DOWNTO 26) <= string_to_std_logic_vector("dc is ");
						txdataq(25 DOWNTO 20) <= strdc0 & X"20";
						txdataq(19 DOWNTO 14) <= strdc1 & X"20";
						txdataq(14 DOWNTO 9) <= strdc2 & X"20";
						txdataq(9 DOWNTO 4) <= strdc3 & X"20";
						txdataq(3 DOWNTO 2) <= (3 DOWNTO 2 => X"20");
					WHEN 10 => -- return flood alert
						txdataq(31 DOWNTO 17) <= string_to_std_logic_vector("flood alert is ");
						txdataq(16) <= X"30" + conv_std_logic_vector(floatsw, 1);
						txdataq(15 DOWNTO 2) <= (15 DOWNTO 2 => X"20");
					WHEN OTHERS =>
						txdataq(31 DOWNTO 15) <= string_to_std_logic_vector("unknown command!!");
						txdataq(14 DOWNTO 2) <= (14 DOWNTO 2 => X"20");
				END CASE;
			END IF;
		END IF;
	END PROCESS;

	-- normalize, trim duplicated space
	PROCESS (nreset, clock)
		VARIABLE c : INTEGER RANGE 0 TO 63;
		VARIABLE bitspace : std_logic;
	BEGIN
		IF nreset = '0' THEN
			c := 0;
			bitspace := '0';
			txdata <= (txdata'RANGE => X"20");
			txdatatmp <= (txdatatmp'RANGE => X"20");
		ELSIF rising_edge(clock) THEN
			IF txtrimtrig = '1' THEN
				txdatatmp <= txdataq;
				txdata <= (txdata'RANGE => X"20");
				c := 32;
				bitspace := '1';
			ELSE
				IF c /= 0 THEN
					IF bitspace /= '1' OR (txdatatmp(0) /= X"20" AND txdatatmp(0) /= X"00") THEN
						txdata <= txdatatmp(0) & txdata(31 DOWNTO 1);
					END IF;
					txdatatmp <= X"00" & txdatatmp(31 DOWNTO 1);
					IF txdatatmp(0) = X"20" OR txdatatmp(0) = X"00" THEN
						bitspace := '1';
					ELSE
						bitspace := '0';
					END IF;
					c := c - 1;
				END IF;
			END IF;
		END IF;
	END PROCESS;

	rs232_0 : rs232_ptc_tc PORT MAP(
		nreset => nreset,
		clock => clock,
		rxd => rxd,

		txframetrig => txframetrig,
		txbuffer => txdata,
		txd => txd,
		rxframetrig => rxframetrig,
		rxbuffer => rxdata
	);

END RTL;