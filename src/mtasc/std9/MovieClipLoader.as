//****************************************************************************
// ActionScript Standard Library
// MovieClipLoader object
//****************************************************************************

intrinsic class MovieClipLoader
{
	var checkPolicyFile : Boolean;

	function MovieClipLoader();

	function addListener(listener:Object):Boolean;
	function getProgress(target:Object):Object;
	function loadClip(url:String, target:Object):Boolean;
	function removeListener(listener:Object):Boolean;
	function unloadClip(target:Object):Boolean;
}