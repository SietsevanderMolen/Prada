with GNAT.OS_Lib;

package Curl is
   CURLE_COULDNT_RESOLVE_HOST : exception;
   CURLE_COULDNT_CONNECT      : exception;
   CURLE_HTTP_RETURNED_ERROR  : exception;
   UnhandledException         : exception;

   function DoHTTPRequest
      (Url : String)
      return String;

   procedure DownloadFile
      (Url  : String;
       Dest : String);

private
   function InvokeCurl
      (Args : GNAT.OS_Lib.Argument_List)
      return String;
end Curl;
