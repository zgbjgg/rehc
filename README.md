rehc
====

REHC remote control and monitor for applications

description
====

REHC allows control and monitoring applications remotely, if the application was down, RHEC could restart it.
RHEC needs the protocol (rsh or ssh) configured properly. You must able to log into the remote hosts, members of the 
cluster wothout a password prompt.


install instructions
====

1) Clone the project
				
				$ git clone https://jorgegarrido@github.com/jorgegarrido/rehc.git

2) Into REHC

				$ cd rehc
				
3) Configure vm.args for REHC

Under directory etc, in the file vm.args, you can configure the node's sname and node's cookie for the 
master node REHC, also you can configure the protocol of connection between RHEC and the host to monitor, 
it could be ssh or rsh.

4) Configure rehc.config for RHEC

Under directory etc, in the file rehc.config, you can set the host short name and ip, cookie and node's name
for each host in your cluster. The attribute rehc_config_dir allows specify the path to your .rhec files
which are defined to configure parmeters of the application.

5) Configure .rhec files

A .rhec files are configured with a simple parameters, you can define one file per one application to 
monitoring, the structure of a file contains the nexts flags and looks like this:

				-app "Name" # Name of your application.
				-start "Start" # The command to start the application.
				-stop "Stop" # The command to stop the application.
				-test "Test" # The command to test if the application is running
				-off "Off" # When the application is down, the message. 
				-node "Node" # Host short name member of the cluster which the application resides.

6) Compile and start

When the configuration is complete:
				
				$ make all
				
make all, compile and start RHEC 

Congratulations!! RHEC is monitoring and control your remote applications, take a rest and enjoy it!!








