

#Module emmap#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-mmap_file">mmap_file()</a>##



<pre>mmap_file() = #file_descriptor{}</pre>



###<a name="type-open_option">open_option()</a>##



<pre>open_option() = read | write | direct | lock | nolock | private | shared | nocache</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-4">open/4</a></td><td></td></tr><tr><td valign="top"><a href="#position-2">position/2</a></td><td></td></tr><tr><td valign="top"><a href="#pread-3">pread/3</a></td><td></td></tr><tr><td valign="top"><a href="#pwrite-3">pwrite/3</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#simple_test-0">simple_test/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="close-1"></a>

###close/1##




<pre>close(File::<a href="#type-mmap_file">mmap_file()</a>) -> ok</pre>
<br></br>


<a name="open-2"></a>

###open/2##




`open(FileName, Options) -> any()`

<a name="open-4"></a>

###open/4##




<pre>open(File::string(), Offset::pos_integer(), Length::pos_integer(), Options::[<a href="#type-open_option">open_option()</a>]) -> {ok, <a href="#type-mmap_file">mmap_file()</a>} | {error, term()}</pre>
<br></br>


<a name="position-2"></a>

###position/2##




<pre>position(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer() | {bof | cur | eof, Position::integer()}) -> {ok, pos_integer()} | {error, term()}</pre>
<br></br>


<a name="pread-3"></a>

###pread/3##




<pre>pread(File::<a href="#type-mmap_file">mmap_file()</a>, Offset::pos_integer(), Length::pos_integer()) -> {ok, binary()} | {error, term()} | eof</pre>
<br></br>


<a name="pwrite-3"></a>

###pwrite/3##




<pre>pwrite(File::<a href="#type-mmap_file">mmap_file()</a>, Position::pos_integer(), Data::binary()) -> ok | {error, term()}</pre>
<br></br>


<a name="read-2"></a>

###read/2##




<pre>read(File::<a href="#type-mmap_file">mmap_file()</a>, Length::pos_integer()) -> {ok, binary()} | {error, term()} | eof</pre>
<br></br>


<a name="simple_test-0"></a>

###simple_test/0##




`simple_test() -> any()`

