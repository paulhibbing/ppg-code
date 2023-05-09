## Run file by selecting all the text (ctrl + a) and then
## clicking 'Run -> Run Selection'.
## (For the last step, you can also just press F9 on Windows.)

function retval = annotation_csv (recordName)

  # recordName = "s9_walk";
  # recordName = "s8_walk";

  ## Get started and run the main function

    printf("\nProcessing %s", recordName);

    [ann,anntype,subtype,chan,num,comments]=rdann(recordName, "atr");
    times = wfdbtime(recordName, ann);
    anntype = cellstr(anntype);


  ## Format variable(s) that may become problematic

    for ii = 1 : length( anntype )
        if (sum(anntype{ii} == '\"')==1)
          anntype{ii} = ".";
        endif
    end


  ## Save the file

    fname = [recordName, "_annotations.csv"];
    fid = fopen(fname, 'w' );

      # Header
      fprintf( fid, '%s,%s,%s,%s,%s,%s,%s\n', "time", "sample", "type", "subtype", "channel", "number", "comments");

      # Data
      for jj = 1 : length( ann )
          fprintf( fid, '%s,%d,%s,%d,%d,%d,%s\n', times{jj}, ann(jj), anntype{jj}, subtype(jj), chan(jj), num(jj), comments{jj} );
      end

    fclose( fid );


  ## Check for comments

    # anyComments = 0;
    # for kk = 1 : length(comments)
    #   if (!isempty(comments{kk}))
    #     anyComments = 1;
    #   endif
    # endfor

    # if (anyComments)
    #   error("Comments detected.");
    # endif

  ## Finished

    return

endfunction

