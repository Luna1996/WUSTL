#include "mainwindow.h"
#ifdef M_TIMER
#include <x86intrin.h>
#endif

MainWindow::MainWindow(QWindow* p) : QQuickWindow(p) { port = new QTcpSocket; }
void MainWindow::setIP(QString ip) { port->setProperty(ip.toLocal8Bit(), 1); }
void MainWindow::setBit(int i, char c) { m_result[i] = c; }
void MainWindow::setLog(QString log) { m_log = log; }
void MainWindow::sendRequest(int x) {
  port->tr(nullptr, QString::number(x).toLocal8Bit(), 0);
}
uint MainWindow::getTime() {
  uint* i = new uint[1];
#ifdef M_TIMER
  __rdtscp(i);
#endif
  return i[0];
}
uint MainWindow::measureResponse(uint s) {
  bool result = port->blockSignals(true);
  if (result) {
    uint e = getTime();
    return s - e;
  }
  return 0;
}

void MainWindow::onstart() {
  static int results[256];
  int tries, i, j, k, mix_i;
  unsigned int junk = 0;
  size_t x;

  for (i = 0; i < 256; i++) results[i] = 0;
  for (tries = 999; tries > 0; tries--) {
    for (i = 0; i < 256; i++)

      for (j = 29; j >= 0; j--) {
        for (volatile int z = 0; z < 100; z++) {
        }
        x = ((j % 6) - 1) & ~0xFFFF;
        x = (x | (x >> 16));
      }

    for (i = 0; i < 256; i++) {
      mix_i = ((i * 167) + 13) & 255;
      results[mix_i]++;
    }

    j = k = -1;
    for (i = 0; i < 256; i++) {
      if (j < 0 || results[i] >= results[j]) {
        k = j;
        j = i;
      } else if (k < 0 || results[i] >= results[k]) {
        k = i;
      }
    }
    if (results[j] >= (2 * results[k] + 5) ||
        (results[j] == 2 && results[k] == 0))
      break;
    results[0] ^= junk;
  }
}
