VHDL Code
Receive: PROCESS(clock)
BEGIN

IF rising_edge(clock) THEN  --At the rising edge of the clock do the following

  CASE Rx_state IS  --FSM to receive bytes of data via an RS232 wire

  WHEN RxIdle =>  --System waits in idle state until Rx goes low
  current_rx_byte <= "00000000";  --Initialises current byte to be empty
  IF(Rx = '0') THEN
  rx_clk_counter <= 0  --When Rx goes low, system moves into idle state
  Rx_state <= RxStart;
  ELSE
  Rx_state <= RxIdle;  --If Rx remains high, system idles
  END IF;

  WHEN RxStart =>
  IF(rx_clk_counter = (xrx_clks_per_bit/2)) THEN --When in start state, system counts a half bit period and checks if line 1s still low
    IF Rx = '0' THEN  --If the start bit is still low then start is validated and system moves to data state
    rx_clk_counter <= 0;
    Rx_state <= RxData;
    ELSE
    Rx_state <= RxIdle; --If start bit is not low, then start is invalid and system moves back to idle state
    END IF;
  ELSE
  rx_clk_counter <= rx_clk_counter + 1; --Counting up clock cycles to half bit period
  Rx_state <= RxStart;
  END IF;

  WHEN RxData =>
  IF(rx_clk_counter < rx_clks_per_bit) THEN --System counts 1 bit peéiod to find middle of data bit
  rx_clk_counter <= rx_clk_counter + 1;
  Rx_state <= RxData;
    ELSE IF(rx_databit_index < 8) THEN --Stores the line value in next bit of array
    current_rx_byte(rx_databit_index) <= Rx; --Repeated for all 2 bits
    rx_databit_index <= rx_databit_index + 1;
    rx_clk_counter <= 0;
    Rx_state <= RxData;
    ELSE
    rx_clk_counter <= 0; --Once system has all 8 data bits
    rx_databit_index <= 0; --Resets clock counter and index
    Tx_ByteLEDs <= current_rx_byte; --Shows current byte on LEDs
    RxBuffer(rx_input_counter) <= current_rx_byte; --Stores byte in appropriate location in receive buffer
    rx_input_counter <= rx_input_counter + 1;
    Rx_state <= RxStop; --Moves to stop state
    END IF;
  END IF:

  WHEN RxStop => --When in stop state, system counts 1 bit period and validates stop bit
  IF(rx_clk_counter < rx_clks_per_bit) THEN
  rx_clk_counter <= rx_clk_counter +1;
  Rx_state <= RxStop;
  ELSE
  Rx_state <= RxIdle; --Once detected, moved back to idle state
  END IF;

  WHEN OTHERS =>
  Rx_state <= RxIdle; --Error handler, to reset system to idle

  END CASE;


  IF (RxBuffer(0) = x"77") THEN --IF a write operation is required, expect 6 inputs (2 extra for byte to write)
  rx_expected_inputs <= 8;
  ELSE
  rx_expected_inputs <= 6; --Else just expect 6
  END IF;
  --After all 8 inputs, set next stage flags--
   
  IF(rx_input_counter = rx_expected_inputs) THEN
  rx_inputs_complete <= '1'; --After 8 inputs, tell next process to start
  rx_input_counter <= 0; --Reset inputs counter
  END IF;
  IF (buffer_index = rx_expected_inputs - 1) THEN --If then next stage completes the conversions
  rx_inputs_complete <= '0'; --Reset the flag
  END IF;
 END IF;

END PROCESS Receive;


Converter: PROCESS(clock, rx_inputs_complete) --Process is activated when the all © inputs are stored in recéive buffer
BEGIN
  
  IF rising_edge(clock) THEN
    
    CASE Conv_state I5 --FSM converts ASCII inputs to hex values to be interpretted by flash device
    
    WHEN ConvIdle => --Process idles & waits for inputs to be complete
    conversion_complete <= 'O'; --While system idles, the conversions are not complete, therefore the next process is held
    IF(rx_inputs_complete = '1') THEN --If inputs are complete move to next state and begin converting
    Conv_state <= ConvProcessing;
    END IF;

    WHEN ConvProcessing =>
    IF (buffer_index < rx_expected_inputs-1) THEN --Cycle through the 2nd-8th received byte and convert from ASCII values to hex values
      IF RxBuffer(buffer_index+1}) <= "00110000" THEN --IF 0 (30) --Converting ASCII values to hex values and storing in hex buffer
      HexBuffer(buffer index) <= x"0";
      buffer_index <= buffer_index+1; --0000
        ELSE IF RxBuffer (buffer_index+1) <= "00110001" THEN --IF 1 (31)
        HexBuffer(buffer index) <= x"1"; --0001
        buffer_index <= buffer_index+1;
          ELSE IF RxBuffer(buffer_index+-1) <= "00110010" THEN --IF 2 (32)
          HexBuffer(buffer_index) <= x"2"; --0010
          buffer_index <= buffer_index+1;
            ELSE IF RxBuffer(buffer_index+1) <= "00110011" THEN --IF 3 (33)
              HexBuffer(buffer_index) <= x"3"; --0011
              buffer_index <= buffer_index+ 1;
                ELSE IF RxBuffer(buffer_index+1) <= "00110100" THEN --IF 4 (34)
                HexBuffer(buffer_index) <= x"4"; --0100
                buffer_index <= buffer_index+1;
                  ELSE IF RxBuffer(buffer_index+1) <= "00110101" THEN --IF 5 (35)
                  HexBuffer(buffer_index) <= x"5"; --0101
                  buffer_index <= buffer_index+1;
                    ELSE IF RxBuffer(buffer_index+1) <= "00110110" THEN --IF 6 (36)
                    HexBuffer(buffer_index) <= x"6"; --0110
                    buffer_index <= buffer_index+1;
                      ELSE IF RxBuffer(buffer_index+1) <= "00110111" THEN --IF 7 (37)
                      HexBuffer (buffer_index) <= x"7"; --0111
                      buffer_index <= buffer_index+1;
                        ELSE IF RxBuffer(buffer_index+1) <= "00111000" THEN --IF 8 (38)
                        HexBuffer(buffer_index) <= x"8"; --1009
                        buffer_index <= buffer_index+1;
                          ELSE IF RxBuffer(buffer_index+1) <= "00111001" THEN --IF 9 (39)
                          HexBuffer(buffer_index) <= x"9"; --1001
                          buffer_index <= buffer_index+1;
                            ELSE IF RxBuffer(buffer_index+1) <= "01000001" THEN --IF A (41)
                            HexBuffer(buffer_index) <= x"A"; --1010
                            buffer_index <= buffer_index+1;
                              ELSE IF RxBuffer(buffer_index+1) <= "01000010" THEN --IF B (42)
                              HexBuffer(buffer_index) <= x"B"; --1011
                              buffer_index <= buffer_index+1;
                                ELSE IF RxBuffer(buffer_index+1) <= "01000011" THEN --IF C (43)
                                HexBuffer(buffer_index) <= x"C"; --1100
                                buffer_index <= buffer_index+1;
                                  ELSE IF RxBuffer(buffer_index+1) <= "01000100" THEN --IF D (44)
                                  HexBuffer(buffer_index) <= x"D"; --1101
                                  buffer_index <= buffer_index+1;
                                    ELSE IF RxBuffer(buffer_index+1) <= "01000101" THEN --IF E (45)
                                    HexBuffer(buffer_index) <= x"E"; --1110
                                    buffer_index <= buffer_index+1;
                                      ELSE IF RxBuffer(buffer_index+1) <= "01000110" THEN --IF F (46)
                                      HexBuffer(buffer_index) <= x"F"; --1111
                                      buffer_index <= buffer_index+1;
      ELSE HexBuffer(buffer_index) <= x"0"; --IF value inputted is not a hex value(9-F) then set as 0
      buffer_index <= buffer_index+1;
                                      END IF;
                                    END IF;
                                  END IF;
                                END IF;
                              END IF;
                            END IF;
                          END IF;
                        END IF;
                      END IF;
                    END IF;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
    ELSE --After buffer has converted all 7 hex values
    buffer_index <= 0; --Reset the buffer index & move to finishing state
    Conv_state <= ConvFinish;
    END IF;

    WHEN ConvFinish => --In finishing state, trigger conversion complete flag for next process and return to idle
    conversion_complete <= '1';
    Conv_state <= ConvIdle;

    WHEN others => --Error handler to return to idle
    Conv_state <= Convidle;

    END CASE;
  END IF;

END PROCESS Converter;



Flash_operation: PROCESS(clock, conversion_complete) --Process is triggered after inputs are complete and have been converted to hex values
BEGIN

IF rising_edge(clock) THEN

  CASE Flash_state IS --Process interprets the inputs and applies the correct mode of operation on the flash memory device

  WHEN FlashIdle => --System idles until the hex conversions are completed, then moves into the mode state
  IF(conversion_complete = '1') THEN
  Flash_state <= FlashMode;
  END IF;

  WHEN FlashMode => --Mode state interprets the first input and determines which mode of operation to execute
  IF RxBuffer(0) = "01100101" THEN --IF input 1 was an ‘e', move to erase state
  Flash_state <= FlashErase;
    ELSE IF RxBuffer(0) = "01110111" THEN --IF input 1 was a 'w', move to write state
    Flash_state <= FlashWrite;
      ELSE IF RxBuffer(0) = "01110010" THEN --If input 1 was a ‘r', move to read state
      Flash_state <= FlashRead;
      END IF;
    END IF;
  END IF;


  WHEN FlashErase => --IF an erase operation was selected
  bidir_ enable <= '1'; --Enable tristate flash data bus signal (write_data_ out) to drive flash data bus
  flash_reset <= '1'; --Set the required reset, output enable and write enable for first stage of erase operation
  flash_oe <= '1";
  flash_we <= '0';
  flash_address <= "00" & x"00AAA"; --Set the address and data for first stage of erase command cycle
  write_data_out<= x"AA";
  IF(erase_clk_counter < 2) THEN --System sets inputs and holds for 40ns/2 clock cycles (35ns min on data sheet)
  erase_clk_counter <= erase_clk_counter + 1;
  ELSE
  flash_we <= '1'; --Inputs are reset and next data and addresses are set for cycle 2
  flash_address <= "00" & x"00555";
  write_data_out <= x"55";
    IF(erase_clk_counter < 4) THEN --System waits for another 2 clock cycles and enables erase cycle 2
    erase_clk_counter <= erase_clk_counter + 1;
    ELSE
    flash_we <= 'O';
      IF(erase_clk_counter < 6) THEN
      erase_clk_counter <= erase_clk_counter + 1;
      ELSE --Inputs are reset and next data and addresses are set for cycle 3
      flash_we <= '1';
      flash_address <= "00" & x"00AAA";
      write_data_out <= x"80";
        IF(erase_clk_counter < 8) THEN --System sets inputs and enables erase cycle 3 for 2 clock cycles (30 ns min)
        erase_clk_counter <= erase_clk_counter + 1;
        ELSE
        flash_we <= 'O';
          IF(erase_clk_counter < 10) THEN
          erase_clk_counter <= erase_clk_counter + 1;
          ELSE
          flash_we <= '1'; --Repeated for erase cycle 4
          flash_address <= "00" & x"00AAA";
          write_data_out <= x"AA";
            IF(erase_clk_counter < 12) THEN
            erase_clk_counter <= erase_clk_counter + 1;
            ELSE
            flash_we <= '0';
              IF(erase_clk_counter < 14) THEN
              erase_clk_counter <= erase_clk_counter + 1;
              ELSE
              flash_we <= '1';
              flash_address <= "00" & x"00555"; --Repeated for erase cycle 5
              write_data_out <= x"55";
                IF(erase_clk_counter < 16) THEN
                erase_clk_counter <= erase_clk_counter + 1;
                ELSE
                flash_we <= '0';
                  IF(erase_clk_counter < 18) THEN
                  erase_clk_counter <= erase_clk_counter + 1;
                  ELSE
                  flash_we <= '1'; --Repeated for erase cycle 6
                  flash_address <= "01" & HexBuffer(0) & HexBuffer(1) & HexBuffer(2) & HexBuffer(3) & HexBuffer(4); --Erase cycle 6 contains address location to be erased, determined
                  write_data_out <= x"30";
                  erase_cycle_complete <= '1';
                    IF(erase_clk_counter < 20) THEN
                    erase_clk_counter <= erase_clk_counter + 1;
                    ELSE
                    flash_we <= '0'; --Sector is erased and inputs are reset
                      IF(erase_clk_counter < 22) THEN
                      erase_clk_counter <= erase_clk_counter + 1;
                      ELSE
                      --Clean up--
                      flash_we <= ‘1';
                      erase clk counter <= 0; --Counters are reset
                      bidir_enable <= '0'; --Tristate signal disabled and system returns to idle
                      erase_cycle_complete <= '0';
                      Flash_state <= FlashIdle;
                      END IF;
                    END IF;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;
    END IF;
  END IF;


  WHEN FlashWrite => --IF user commanded a write operation, the following state is executed
  bidir_enable <= '1'; --Enable tristate flash data bus signal (write data_out) to drive flash data bus
  flash_reset <= '1'; --Flash inputs are set
  flash_oe <= '1';
  flash_address <= "00" & x"0OAAA"; --Flash write unlock cycle data and addresses are set
  write_data_out<= x"AA";
    IF(write _clk_counter < 4) THEN
    write_clk_counter <= write_clk_ counter + 1;
    ELSE --System waits for 2 clock cycles to write data to addresses and resets for an exta 2 cycles (35ns min)
    flash_we <= 'O';
      IF(write_clk_counter < 8) THEN
      write_clk_counter <= write clk counter + 1;
      ELSE --System sets the second unlock cycle data and addresses
      flash_we <= '1';
      flash_address <= "00" & x"00555";
      write_data_out <= x"55";
        IF(write _clk_counter < 12) THEN --Waits 4 clock cycles and write to flash
        write_clk_counter <= write_clk_counter + 1;
        ELSE
        flash_we <= '0';
          IF(write_clk_counter < 16) THEN
          write_clk_counter <= write_clk_counter + 1;
          ELSE --System sets third write cycle data and addresses
          flash_we <= ‘1';
          flash_address <= "00" & x"0OAAA";
          write_data_out <= x"A0";
            IF(write_clk_counter < 20) THEN
            write_clk_counter <= write_clk_counter + 1;
            ELSE --Waits 4 clock cycles and writes to flash
            flash_we <= '0';
              IF(write_clk_counter < 24) THEN
              write_clk_counter <= write_clk_counter + 1;
              ELSE --System sets fourth write cycle data and addresses
              flash_we <= '1';
              flash_address <= "01" & HexBuffer(0) & HexBuffer(1) & HexBuffer(2) & HexBuffer(3) & HexBuffer(4); --Addresses are determined by user inputs
              write_data_out<= HexBuffer(5) & HexBuffer(6); --Data determined by user inputs
              write_cycle complete <= '1’;
                IF(write_clk_counter < 28) THEN
                write_clk_counter <= write_clk_counter + 1;
                ELSE --System completes cycle
                flash_we <= '0';
                  IF(write_clk_ counter < 32) THEN
                  write_clk_counter <= write_clk_counter + 1;
                  ELSE
                  --Clean up--
                  flash_we <= '1'; --Resets inputs and counters
                  write_clk_counter <= 0;
                  bidir_enable <= 'O'; --Resets tristate signal and returns to idle
                  write_cycle_complete <= '0';
                  Flash_state <= FlashIdle;
                  END IF;
                END IF;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;
    END IF;
  
  WHEN FlashRead => --IF user requests a read operation, the following state is executed
  bidir_enable <= '0'; --Tristate signal is disabled
  flash_reset <= '1'; --Flash inputs are set
  flash_we <= '1';
  flash_oe <= ‘1';
  flash_address <= "01" & HexBuffer(0) & HexBuffer(1) & HexBuffer(2) & HexBuffer(3) & HexBuffer(4); --Flash address of memory location to be read is set according to user input
    IF(read_clk_counter < 2) THEN --System waits for 2 clock cycles (35ns min) and outputs data in memory
    read_clk_counter <= read_clk_counter + 1;
    ELSE
    flash_oe <= '0';
      IF(read_clk_counter < 4) THEN --After 2 clock cycles the data is valid
      read_clk_counter <= read_clk_counter + 1;
      ELSE
      Data_LEDs <= read data_in; --Data on bus is copied to the LEDS & stored as output byte
      Tx_output_byte <= read_data_in;
      read_cycle_complete <= '1';
        IF(read_clk_counter < 6) THEN
        read_clk_counter <= read_clk_counter + 1;
        ELSE
        flash_oe <= '1'; --Flash inputs are reset
        read_cycle_complete <= '0';
        read_clk_counter <= 0 --Counters reset and bidirectional signal disabled
        bidir_enable <= '0';
        Flash_state <= FlashIdle;
        END IF;
      END IF;
    END IF;
  
  WHEN others => --Error handler to return to idle
  Flash_state <= FlashIdle;

  END CASE;

END IF;

END PROCESS Flash_operation;


Transmit_read_result: PROCESS(clock,read_cycle_complete,erase_cycle_complete,write_cycle_complete) --Process to transmit result of read operation back to user
BEGIN

IF rising_edge(clock) THEN

  IF(read_cycle_complete = '1') THEN --Set internal flags if flash processes have been completed
  TxRead <= '1';
    ELSE IF(erase_cycle_complete = '1') THEN
    TxErase <= '1';
      ELSE IF(write_cycle_complete = '1') THEN
      TxWrite <= '1';
      END IF;
    END IF;
  END IF;

  CASE Tx_State IS --FSM to transmit message back to user via RS232
  
  WHEN TxIdle =>
  Tx <= '1'; --While in an idle state drive the output line high
  IF(TxRead = '1' OR TxErase = '1' OR TxWrite = '1' OR TxReadMessage = '1') THEN --When any flash operation completed, move to start state
  Tx_State <= TxStart;
  END IF;

  WHEN TxStart =>
  Tx <='0'; --When arriving at start state, set output line low to act as start bit for receiver
  IF(Tx_ClockCounter < Tx_ClocksPerBit) THEN
  Tx_ClockCounter <= Tx_ClockCounter + 1; --After 1 bit period move to the data state of whichever operation has completed
  Tx_State <= TxStart;
  ELSE
  Tx_ClockCounter <= 0;
    IF(TxRead = '1') THEN
    Tx_State <= TxReadData;
      ELSE IF(TxErase = '1') THEN
      Tx_State <= TxEraseData;
        ELSE IF(TxWrite = '1') THEN
        Tx_State <= TxWriteData;
          ELSE IF(TxReadMessage = '1') THEN
          Tx_State <= TxReadDataMessage;
          END IF;
        END IF;
      END IF;
    END IF;
  END IF;

  WHEN TxEraseData => --If erase operation has been completed
  Current_erase_message <= EraseMessaae(EraseMessaaeIndex); --Cycle through each bit of each erase messace letter and transmit it onto the Tx line
  Tx <= Current_erase_message(EraseCharacterIndex);
  IF(Tx_ClockCounter < Tx_ClocksPerBit) THEN
  Tx_ClockCounter <= Tx_ClockCounter + 1;
  Tx_State <= TxEraseData;
  ELSE
  Tx_ClockCounter <= 0;
    IF(EraseCharacterIndex < 7) THEN --Go through every letter and increment by 1
    Tx_ClockCounter <= 0;
    EraseCharacterIndex <= EraseCharacterIndex + 1;
    Tx_State <= TxEraseData;
    ELSE --When each byte sent, move to stop state
    Tx_ClockCounter <= 0;
    EraseCharacterIndex <= 0;
    Tx_state <= TxStop;
    END IF;
  END IF;

  WHEN TxWriteData => --If write operation has been completed
  Current_write_message <= WriteMessage(WriteMessageIndex); --Cycle through each bit of each write message letter and transmit it onto the Tx line
  Tx <= Current_write_message(WriteCharacterIndex);
  IF(Tx_ClockCounter < Tx_ClocksPerBit) THEN
  Tx_ClockCounter <= Tx_ClockCounter + 1;
  Tx_State <= TxWriteData;
  ELSE
  Tx_ClockCounter <= 0;
    IF(WriteCharacterIndex < 7) THEN
    Tx_ClockCounter <= 0;
    WriteCharacterIndex <= WriteCharacterIndex + 1;
    Tx_State <= TxWriteData;
    ELSE --When each byte been sent, move to stop state
    Tx_ClockCounter <= 0;
    WriteCharacterIndex <= 0;
    Tx_state <= TxStop;
    END IF;
  END IF;

  
  WHEN TxReadDataMessage => --When if read operation has been completed
  Current_read_message <= ReadMessage(ReadMessageIndex-1); --Cycle through each bit of each read message letter and transmit it onto the Tx line
  Tx <= Current_read_message(ReadCharacterIndex);
  IF(Tx_ClockCounter < Tx_ClocksPerBit) THEN
  Tx_ClockCounter <= Tx_ClockCounter + 1;
  Tx_State <= TxReadDataMessage;
  ELSE
  Tx_ClockCounter <= 0;
    IF(ReadCharacterIndex < 7) THEN
    Tx_ClockCounter <= 0;
    ReadCharacterIndex <= ReadCharacterIndex + 1;
    Tx_State <= TxReadDataMessage;
    ELSE
    Tx_ClockCounter <= 0; --When each byte has been sent, move to stop state
    ReadCharacterIndex <= 0;
    Tx_state <= TxStop;
    END IF;
  END IF;
  
  
  WHEN TxReadData => --When in data state, set the output line equal to the bit of data at the current index of the data byte
  Tx <= Tx_output_byte(Tx_DataBit_Index);
  IF(Tx_ClockCounter < Tx_ClocksPerBit) THEN --Count for 1 bit period
  Tx_ClockCounter <= Tx_ClockCounter + 1;
  Tx_State <= TxReadData;
  ELSE
  Tx_ClockCounter <= 0;
    IF(Tx_DataBit_Index < 7) THEN --Repeat for the remaining 7 bits of the output byte
    Tx_ClockCounter <= 0;
    Tx_DataBit_Index <= Tx_DataBit_Index + 1;
    Tx_State <= TxReadData;
    ELSE --Once all 8 bits have been transmitted, move to the stop state
    Tx_ClockCounter <= 0;
    Tx_DataBit_Index <= 0;
    TxReadComplete <= '1';
    TxRead <= 'O';
    Tx_State <= TxStop;
    END IF;
  END IF;


  WHEN TxStop=> --When arriving at stop state, drive line high to act as the stop bit for the receiver
  Tx <= '1';
  IF(Tx_ClockCounter < Tx_ClocksPerBit) THEN --Count for 1 bit period
  Tx_ClockCounter <= Tx_ClockCounter + 1;
  Tx_State <= TxStop;
    ELSE IF(EraseMessageIndex < 18 AND TxErase = '1') THEN --System checks whether all bytes have been sent from each success message
    Tx_ClockCounter <= 0;
    EraseMessageIndex <= EraseMessageIndex + 1;
    Tx_State <= TxIdle;
      ELSE IF(WriteMessageIndex < 18 AND TxWrite = '1') THEN
      Tx_ClockCounter <= 0;
      WriteMessageIndex <= WriteMessageIndex + 1;
      Tx_State <= TxIdle;
        ELSE IF (ReadMessageIndex < 18 AND TxReadComplete = '1') THEN
        TxReadMessage <= '1';
        Tx_ClockCounter <= 0;
        ReadMessageIndex <= ReadMessageIndex + 1;
        Tx_State <= TxIdle;
          ELSE --If the message is complete reset all counter
          EraseCharacterIndex <= 0;
          EraseMessageIndex <= 0;
          WriteCharacterIndex <= 0;
          WriteMessageIndex <= 0;
          ReadCharacterIndex <= 0;
          ReadMessageIndex <= 0;
          TxReadComplete <= '0';
          TxReadMessage <= '0';
          Tx_ClockCounter <= 0;
          TxRead <= '0';
          TxErase <= '0';
          TxWrite <= '0';
          Tx_State <= TxIdle; --Return to Idle state
          END IF;
        END IF;
      END IF;
    END IF;
  
  WHEN others => --Error handler to return to idle sate
  Tx_ State <= TxIdle;

  END CASE;
  
 END IF;

END PROCESS Transmit_read_result;

END rtl;

