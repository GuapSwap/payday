package configs

import configs.node.PaydayNodeConfig
import configs.parameters.PaydayParameters
import scala.util.{Try, Success, Failure}
import com.google.gson.{Gson, GsonBuilder}
import java.io.File
import java.io.FileReader

/**
 * Class representing the configuation settings.
 * 
 * @param node
 * @param parameters
 */
case class PaydayConfig(
    val node: PaydayNodeConfig,
    val parameters: PaydayParameters
  ) 

object PaydayConfig {
    
    /**
     * Loads the PaydayConfig from the configuration file
     * 
     * @param configFileName
     * @return Try[PaydayConfig]
     */
    def load(configFilePath: String): Try[PaydayConfig] = Try {

        // Load the file
        val configFile: File = new File(configFilePath);

        // Read the file
        val configReader: FileReader = new FileReader(configFile);

        // Create Gson object to parse json
        val gson: Gson = new GsonBuilder().create();

        // Parse the json and create the PaydayConfig object
        val config: PaydayConfig = gson.fromJson(configReader, classOf[PaydayConfig]);
        config
    }

}