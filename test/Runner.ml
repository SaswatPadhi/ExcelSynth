let () =
  Alcotest.run "ExcelSynth"
               [
                 "Test_Synthesizer", Test_Synthesizer.all ;
                 "Test_Driver",      Test_Driver.all ;
               ]