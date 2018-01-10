package io.trosa.examregister

/*
* (c) iomonad - <me@trosa.io>
* Desc: Automated registration for 42.fr C exams.
* Deploy with crontab: 02 0 * * WED java -jar register.jar -u foo -p bar -d ~/geckodriver
* Disclaimer: Use at your own risk.
*/

import com.xenomachina.argparser.*
import org.openqa.selenium.By
import org.openqa.selenium.WebElement
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.firefox.FirefoxOptions
import org.openqa.selenium.firefox.FirefoxProfile
import java.io.File
import kotlin.system.exitProcess

@JvmSuppressWildcards
private class ParseArgs(parser: ArgParser) {
    val geckodriver: String by parser.storing("-d", "--driver"
            , help = "Absolute geckodriver path") { toString() }
            .addValidator {
                if (!File(value).isFile)
                    throw InvalidArgumentException(
                            "Gecko driver should be a binary file")
                exitProcess(status = 1)
            }
    val username: String by parser.storing("-u", "--username"
            , help = "42.fr Intranet Username") { toString() }
            .addValidator {
                when(value.length < 8) {
                    true -> throw InvalidArgumentException(
                            "Invalid username length")
                }
                exitProcess(status = 1)
            }
    val password: String by parser.storing("-p", "--password"
            , help = "42.fr Intranet Password") { toString() }
    val basename: String by parser.storing("-b", "--basepath"
            , help = "42.fr url basename") { toString() }
            .default("https://intra.42.fr")
    val eventype: String by parser.storing("-e", "--event"
            , help = "Event to subscribe") { toString() }
            .default("Exam C")
}

object ExamRegister {
    @JvmStatic
    fun main(argv: Array<String>) = mainBody("eventregister") {

        val fmt: HelpFormatter = DefaultHelpFormatter(prologue = "(c) 2018 iomonad - 42.fr Event register ")

        ParseArgs(ArgParser(argv, helpFormatter = fmt)).run {

            /* Load selenium driver */
            System.setProperty("webdriver.gecko.driver", geckodriver)

            val cred = HashMap<String, String>()
            cred.put("user_login", username)
            cred.put("user_password", password)

            val options: FirefoxOptions? = FirefoxOptions()
                    .setProfile(FirefoxProfile())
            val driver = FirefoxDriver(options)

            driver.run {
                navigate().to(basename)
                cred.forEach { (k, v) ->
                    findElement(By.id(k)).sendKeys(v)
                }
                findElement(By.name("commit")).click()
            }

            /* Retrieve list of events */
            val events: List<WebElement> =
                    driver.findElements(By.ByClassName("event-item"))

            events.forEachIndexed({ i: Int, event: WebElement ->

                val descu: WebElement = event.findElement(By.ByClassName("event-subname"))

                /* Regex is Harmful */
                if (descu.text == eventype) {

                    /* Set position on button */
                    val p1: WebElement = event.findElement(By.ByClassName("event-metadata"))
                    val p2: WebElement = p1.findElement(By.ByClassName("event-button"))

                    /* Final event path*/
                    val ev = "https://profile.intra.42.fr%s"
                            .format(p2.getAttribute("data-url"))

                    driver.run { navigate().to(ev) }

                    val exam = driver.findElement(By.xpath("//footer"))
                            .findElement(By.ByClassName("btn-primary"))

                    when (exam.isEnabled) {
                        true -> when (exam.text) {
                                "The event is full" -> throw SystemExitException("Event is full", 1)
                                else -> {
                                    exam.click()
                                    println("Registered to Exam C for event: %d ~ Status: %s"
                                            .format(i, exam.text))
                                }
                            }
                        false -> throw SystemExitException("Event is full", 1)
                    }
                }
            })
        }
    }
}