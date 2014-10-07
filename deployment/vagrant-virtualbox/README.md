# Using Vagrant and Virtualbox 

README's for development and production environments exist. This
page is solely for building a list of the environment variables 
that modify how TFB launches your Virtuabox virtual machines. 

| Name                             | Values              | Purpose                  |
| :------------------------------- | :------------------ | :----------------------- | 
| `TFB_VB_SHOW`                    | `true,false`        | Show the VM in a window when running? Default is false
| `TFB_VB_ARCH`                    | `64,32`             | Used to force TFB to run a 32-bit virtual machine. This is unsupported, as many of the framework binaries we download are 64-bit only and will not launch. If you cannot run a 64-bit VM, then we recommend using the Amazon AWS provider instead of using this variable
| `TFB_VB_MEM`                     | `<number>` e.g. `2048` | Size of VM's RAM in MB. Default is `2048`
| `TFB_VB_CPU`                     | `<number>` e.g. `2` | Number of host CPUs that the VM can access