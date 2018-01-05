/*
** Since 42.fr disconnect
** user when idle >= 42 minutes,
** I made this little program
** to keep alive connection
** using teensy board,
** piped in a false mouse body.
*/

void setup(void)
{
  Keyboard.begin();
}

void loop()
{
  Keyboard.print("d00m"); 
  delay(1200000); // 20 minutes
}
