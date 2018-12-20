#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QQuickWindow>
#include <QTcpSocket>

class MainWindow : public QQuickWindow {
  Q_OBJECT

  Q_PROPERTY(QString ip MEMBER m_ip)
  Q_PROPERTY(QString result MEMBER m_result)
  Q_PROPERTY(QString log MEMBER m_log)

  QString m_ip, m_result, m_log;
  QTcpSocket* port;

  void setIP(QString ip);
  void setBit(int i, char c);
  void setLog(QString log);

  uint getTime();

  void sendRequest(int x);
  uint measureResponse(uint s);

 public:
  MainWindow(QWindow* p = nullptr);
  Q_INVOKABLE void onstart();
};

#endif  // MAINWINDOW_H