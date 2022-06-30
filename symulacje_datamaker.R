for(.rho in rho){
  sim.n.test("M0", m_01(time), m_01(time), xi["M0to3"], .rho = .rho)
  sim.n.test("M1", m_01(time), m_11(time), xi["M0to3"], .rho = .rho)
  sim.n.test("M2", m_01(time), m_21(time), xi["M0to3"], .rho = .rho)
  sim.n.test("M3", m_01(time), m_31(time), xi["M0to3"], .rho = .rho)
  sim.n.test("M4", m_02(time), m_02(time), xi["M4to7"], .rho = .rho)
  sim.n.test("M5", m_02(time), m_12(time), xi["M4to7"], .rho = .rho)
  sim.n.test("M6", m_02(time), m_22(time), xi["M4to7"], .rho = .rho)
  sim.n.test("M7", m_02(time), m_32(time), xi["M4to7"], .rho = .rho)
}