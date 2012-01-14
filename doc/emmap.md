

#Module emmap#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-4">open/4</a></td><td></td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td></td></tr><tr><td valign="top"><a href="#pread-3">pread/3</a></td><td></td></tr><tr><td valign="top"><a href="#pwrite-3">pwrite/3</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#simple_test-0">simple_test/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="close-1"></a>

###close/1##




`close(File_descriptor) -> any()`

<a name="open-4"></a>

###open/4##




<pre>open(File::string(), Offset::pos_integer(), Length::pos_integer(), Options::[read | write | direct | nocache | private | shared]) -&gt; {ok, term()} | {error, term()}</pre>
<br></br>


<a name="position-2"></a>

###position/2##




`position(File_descriptor, At) -> any()`

<a name="pread-3"></a>

###pread/3##




`pread(File_descriptor, Off, Len) -> any()`

<a name="pwrite-3"></a>

###pwrite/3##




`pwrite(File_descriptor, Off, Data) -> any()`

<a name="read-2"></a>

###read/2##




`read(File_descriptor, Len) -> any()`

<a name="simple_test-0"></a>

###simple_test/0##




`simple_test() -> any()`

