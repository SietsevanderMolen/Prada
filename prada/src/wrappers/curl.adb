with GNAT.Expect;

package body Curl is
   function DoHTTPRequest
      (Url : String)
   return String
   is
      Output : constant String
         := InvokeCurl ((
               1 => new String'("-f"), --  Fail silently and return 22 instead
               2 => new String'("-g"), --  Switch off URL globbing parser
               3 => new String'("-s"), --  Silent mode, no progress, no errors
               4 => new String'(Url)   --  Url to download
            ));
   begin
      return Output;
   end DoHTTPRequest;

   procedure DownloadFile
      (Url  : String;
       Dest : String)
   is
      Output : constant String
         := InvokeCurl ((
               1 => new String'("-f"), -- Fail silently and return 22 instead
               2 => new String'("-g"), -- Switch off URL globbing parser
               3 => new String'("-s"), -- Silent mode, no progress, no errors
               4 => new String'("-o"), -- Output flag
               5 => new String'(Dest), -- Output destination
               6 => new String'(Url)   -- Url to download
            ));
      pragma Unreferenced (Output);
   begin
      null;
   end DownloadFile;

   function InvokeCurl
      (Args : GNAT.OS_Lib.Argument_List)
   return String
   is
      Return_Code : aliased Integer;
      Output : constant String :=
         GNAT.Expect.Get_Command_Output (
            "/usr/bin/curl", Args, "", Return_Code'Access);
   begin
      case Return_Code is
         when 0 =>
            return Output;
         when 6 =>
            raise CURLE_COULDNT_RESOLVE_HOST with
               "Could not resolve host; Name or service not known";
         when 7 =>
            raise CURLE_COULDNT_CONNECT with
               "Failed to connect() to host or proxy.";
         when 22 =>
            raise CURLE_HTTP_RETURNED_ERROR with
               "The HTTP server returned an error code that is >=400";
         when others =>
            raise UnhandledException with
               "Curl returned with " & Return_Code'Img;
      end case;
   end InvokeCurl;
end Curl;
