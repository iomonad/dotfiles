/*
** Avoid 42.fr to disconnect
** sessions that exceed 42mn of
** log time ~
*/

void setup()
{
  Serial.begin(9600);
  delay(1000);
}

void loop()
{
  /* Space is ok */
  Keyboard.print(" ");
  /* Wait for 10 minutes */
  delay(1000 * 60 * 10);
}
