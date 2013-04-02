rehc
====

rehc is an application that provides:

	-monitor for apps on remote host ().
	-statistics about cpu usage, memory consumption and disk space.
	-API web for view info about remote hosts.
	-Real time statistics via websockets.
	-Notifications by email when apps are down or critical usage (cpu, memory and disk).
	-Backup your remote mnesia nodes
	
	
downloading and installing
====

Download rehc from the git repo

		git clone https://github.com/jorgegarrido/rehc
		
Let's into rehc directory

		cd rehc
		
Now like unix/linux applications, install normally by:

		./configure && make && make install
	
preparing the environment
====

To allow rehc monitor your hosts, first you must be able to log into remote hosts without a password confirmation via
ssh or rsh, it will be set in the vm.args file.
For example if my remote host name (hostname -s) is remote_host, then I must be able to log into it via ssh with:

		ssh user@remote_host -p 22
		

when environment is ready, now proceed to configure files for rehc.

configuring files (rehc.config & vm.args)
====

rehc uses two singles files for configuration, vm.args define the node erlang name, cookie for distributed connection
and rsh which defines the protocol used to log into remote host withou a password (it could be rsh or ssh, we 
suggest that use ssh).

rehc.config contains many sections where:

	cluster      - it defines the clusterizable system
	rehc_core    - defines the monitoring options
	rehc_web_api - defines the API parameters for consume web services
	rehc_email   - for email receptor of notifications


the .rehc files
====

.rehc files are files that rehc uses for monitoring remote apps, the structure is similar to program a 
bash script but no loops you need just commands to start, test, stop the remote app. The files are built 
with pairs of data like {key, value}, where key is a reserved word like: app, start, stop, test, off and node,
and data is a string "" containing your command as in bash script, finally comments start with '%'.
The files have the next structure:

	%% Name of your application.
	{app, "my_app"}.

	%% The command to start the application.
	{start, "/usr/local/my_app/bin/my_app start"}.

	%% The command to stop the application.
	{stop, "/usr/local/my_app/bin/my_app-x shutdown"}.

	%% The command to test if the application is running
	{test, "/usr/local/my_app/bin/my_app -x status | wc -l"}.

	%% When the application is down, the message.
	{off, "0"}.
	
	%% Host short name member of the cluster which the application resides.
	{node, "my_remote_hostname_s"}.
	

API

rehc provides a based web API to retrive info about remote hosts, using JSON as rpc, please
refer to rehc_api for more details.


Author

Jorge Garrido <zgbjgg@gmail.com>

LICENSE	


			
			
			
			
