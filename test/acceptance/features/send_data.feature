Feature:
  Send data to remote servers using the framming protocol

  Scenario:
    Given that I want to send data to a server
    Then I encapsulate data according to the RFC
    and I send it to the server
