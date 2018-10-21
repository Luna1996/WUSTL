import m5
from m5.objects import *
import os
gem5_path=os.environ["GEM5"]
import optparse
parser=optparse.OptionParser()
parser.add_option("--prog",type="str",default=None)
(options,args)=parser.parse_args()
program=options.prog
system=System()
system.clk_domain=SrcClockDomain()
system.clk_domain.voltage_domain=VoltageDomain()
isa=m5.defines.buildEnv['TARGET_ISA']
if isa=="x86":
  system.clk_domain.clock='1GHz'
elif isa=="arm":
  system.clk_domain.clock='1.2GHz'
system.mem_mode='timing'
system.mem_ranges=[AddrRange('512MB')]
system.cpu=TimingSimpleCPU()
system.membus=SystemXBar()
system.cpu.icache_port=system.membus.slave
system.cpu.dcache_port=system.membus.slave
system.cpu.createInterruptController()
if isa=='x86':
  system.cpu.interrupts[0].pio=system.membus.master
  system.cpu.interrupts[0].int_master=system.membus.slave
  system.cpu.interrupts[0].int_slave=system.membus.master

system.system_port=system.membus.slave
system.mem_ctrl=DDR3_1600_8x8()
system.mem_ctrl.range=system.mem_ranges[0]
system.mem_ctrl.port=system.membus.master
process=Process()
apps_path="/project/linuxlab/gem5/test_progs/"
if program=="daxpy" and isa=="x86":
  process.cmd=[apps_path+'/daxpy/daxpy_x86']
elif program == "daxpy" and isa == "arm":
  process.cmd = [apps_path + '/daxpy/daxpy_arm']
elif program == "queens" and isa == "x86":
  process.cmd = [apps_path + '/queens/queens_x86']
  process.cmd+=["10 -c"]
elif program=="queens" and isa == "arm":
  process.cmd = [apps_path + 'queens/queens_arm']
  process.cmd += ["10 -c"]

system.cpu.workload=process
system.cpu.createThreads()
root=Root(full_system=False, system=system)
m5.instantiate()
print("Beginning simulation!")
exit_event=m5.simulate()
print('Exiting @ tick because '.format(m5.curTick(), exit_event.getCause()))
