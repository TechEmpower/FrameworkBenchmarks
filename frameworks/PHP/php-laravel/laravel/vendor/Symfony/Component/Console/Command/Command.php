<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Console\Command;

use Symfony\Component\Console\Input\InputDefinition;
use Symfony\Component\Console\Input\InputOption;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Application;
use Symfony\Component\Console\Helper\HelperSet;

/**
 * Base class for all commands.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 *
 * @api
 */
class Command
{
    private $application;
    private $name;
    private $aliases;
    private $definition;
    private $help;
    private $description;
    private $ignoreValidationErrors;
    private $applicationDefinitionMerged;
    private $code;
    private $synopsis;
    private $helperSet;

    /**
     * Constructor.
     *
     * @param string $name The name of the command
     *
     * @throws \LogicException When the command name is empty
     *
     * @api
     */
    public function __construct($name = null)
    {
        $this->definition = new InputDefinition();
        $this->ignoreValidationErrors = false;
        $this->applicationDefinitionMerged = false;
        $this->aliases = array();

        if (null !== $name) {
            $this->setName($name);
        }

        $this->configure();

        if (!$this->name) {
            throw new \LogicException('The command name cannot be empty.');
        }
    }

    /**
     * Ignores validation errors.
     *
     * This is mainly useful for the help command.
     */
    public function ignoreValidationErrors()
    {
        $this->ignoreValidationErrors = true;
    }

    /**
     * Sets the application instance for this command.
     *
     * @param Application $application An Application instance
     *
     * @api
     */
    public function setApplication(Application $application = null)
    {
        $this->application = $application;
        if ($application) {
            $this->setHelperSet($application->getHelperSet());
        } else {
            $this->helperSet = null;
        }
    }

    /**
     * Sets the helper set.
     *
     * @param HelperSet $helperSet A HelperSet instance
     */
    public function setHelperSet(HelperSet $helperSet)
    {
        $this->helperSet = $helperSet;
    }

    /**
     * Gets the helper set.
     *
     * @return HelperSet A HelperSet instance
     */
    public function getHelperSet()
    {
        return $this->helperSet;
    }

    /**
     * Gets the application instance for this command.
     *
     * @return Application An Application instance
     *
     * @api
     */
    public function getApplication()
    {
        return $this->application;
    }

    /**
     * Checks whether the command is enabled or not in the current environment
     *
     * Override this to check for x or y and return false if the command can not
     * run properly under the current conditions.
     *
     * @return Boolean
     */
    public function isEnabled()
    {
        return true;
    }

    /**
     * Configures the current command.
     */
    protected function configure()
    {
    }

    /**
     * Executes the current command.
     *
     * This method is not abstract because you can use this class
     * as a concrete class. In this case, instead of defining the
     * execute() method, you set the code to execute by passing
     * a Closure to the setCode() method.
     *
     * @param InputInterface  $input  An InputInterface instance
     * @param OutputInterface $output An OutputInterface instance
     *
     * @return integer 0 if everything went fine, or an error code
     *
     * @throws \LogicException When this abstract method is not implemented
     * @see    setCode()
     */
    protected function execute(InputInterface $input, OutputInterface $output)
    {
        throw new \LogicException('You must override the execute() method in the concrete command class.');
    }

    /**
     * Interacts with the user.
     *
     * @param InputInterface  $input  An InputInterface instance
     * @param OutputInterface $output An OutputInterface instance
     */
    protected function interact(InputInterface $input, OutputInterface $output)
    {
    }

    /**
     * Initializes the command just after the input has been validated.
     *
     * This is mainly useful when a lot of commands extends one main command
     * where some things need to be initialized based on the input arguments and options.
     *
     * @param InputInterface  $input  An InputInterface instance
     * @param OutputInterface $output An OutputInterface instance
     */
    protected function initialize(InputInterface $input, OutputInterface $output)
    {
    }

    /**
     * Runs the command.
     *
     * The code to execute is either defined directly with the
     * setCode() method or by overriding the execute() method
     * in a sub-class.
     *
     * @param InputInterface  $input  An InputInterface instance
     * @param OutputInterface $output An OutputInterface instance
     *
     * @see setCode()
     * @see execute()
     *
     * @api
     */
    public function run(InputInterface $input, OutputInterface $output)
    {
        // force the creation of the synopsis before the merge with the app definition
        $this->getSynopsis();

        // add the application arguments and options
        $this->mergeApplicationDefinition();

        // bind the input against the command specific arguments/options
        try {
            $input->bind($this->definition);
        } catch (\Exception $e) {
            if (!$this->ignoreValidationErrors) {
                throw $e;
            }
        }

        $this->initialize($input, $output);

        if ($input->isInteractive()) {
            $this->interact($input, $output);
        }

        $input->validate();

        if ($this->code) {
            return call_user_func($this->code, $input, $output);
        }

        return $this->execute($input, $output);
    }

    /**
     * Sets the code to execute when running this command.
     *
     * If this method is used, it overrides the code defined
     * in the execute() method.
     *
     * @param \Closure $code A \Closure
     *
     * @return Command The current instance
     *
     * @see execute()
     *
     * @api
     */
    public function setCode(\Closure $code)
    {
        $this->code = $code;

        return $this;
    }

    /**
     * Merges the application definition with the command definition.
     */
    private function mergeApplicationDefinition()
    {
        if (null === $this->application || true === $this->applicationDefinitionMerged) {
            return;
        }

        $currentArguments = $this->definition->getArguments();
        $this->definition->setArguments($this->application->getDefinition()->getArguments());
        $this->definition->addArguments($currentArguments);

        $this->definition->addOptions($this->application->getDefinition()->getOptions());

        $this->applicationDefinitionMerged = true;
    }

    /**
     * Sets an array of argument and option instances.
     *
     * @param array|InputDefinition $definition An array of argument and option instances or a definition instance
     *
     * @return Command The current instance
     *
     * @api
     */
    public function setDefinition($definition)
    {
        if ($definition instanceof InputDefinition) {
            $this->definition = $definition;
        } else {
            $this->definition->setDefinition($definition);
        }

        $this->applicationDefinitionMerged = false;

        return $this;
    }

    /**
     * Gets the InputDefinition attached to this Command.
     *
     * @return InputDefinition An InputDefinition instance
     *
     * @api
     */
    public function getDefinition()
    {
        return $this->definition;
    }

    /**
     * Gets the InputDefinition to be used to create XML and Text representations of this Command.
     *
     * Can be overridden to provide the original command representation when it would otherwise
     * be changed by merging with the application InputDefinition.
     *
     * @return InputDefinition An InputDefinition instance
     */
    protected function getNativeDefinition()
    {
        return $this->getDefinition();
    }

    /**
     * Adds an argument.
     *
     * @param string  $name        The argument name
     * @param integer $mode        The argument mode: InputArgument::REQUIRED or InputArgument::OPTIONAL
     * @param string  $description A description text
     * @param mixed   $default     The default value (for InputArgument::OPTIONAL mode only)
     *
     * @return Command The current instance
     *
     * @api
     */
    public function addArgument($name, $mode = null, $description = '', $default = null)
    {
        $this->definition->addArgument(new InputArgument($name, $mode, $description, $default));

        return $this;
    }

    /**
     * Adds an option.
     *
     * @param string  $name        The option name
     * @param string  $shortcut    The shortcut (can be null)
     * @param integer $mode        The option mode: One of the InputOption::VALUE_* constants
     * @param string  $description A description text
     * @param mixed   $default     The default value (must be null for InputOption::VALUE_REQUIRED or InputOption::VALUE_NONE)
     *
     * @return Command The current instance
     *
     * @api
     */
    public function addOption($name, $shortcut = null, $mode = null, $description = '', $default = null)
    {
        $this->definition->addOption(new InputOption($name, $shortcut, $mode, $description, $default));

        return $this;
    }

    /**
     * Sets the name of the command.
     *
     * This method can set both the namespace and the name if
     * you separate them by a colon (:)
     *
     *     $command->setName('foo:bar');
     *
     * @param string $name The command name
     *
     * @return Command The current instance
     *
     * @throws \InvalidArgumentException When command name given is empty
     *
     * @api
     */
    public function setName($name)
    {
        $this->validateName($name);

        $this->name = $name;

        return $this;
    }

    /**
     * Returns the command name.
     *
     * @return string The command name
     *
     * @api
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * Sets the description for the command.
     *
     * @param string $description The description for the command
     *
     * @return Command The current instance
     *
     * @api
     */
    public function setDescription($description)
    {
        $this->description = $description;

        return $this;
    }

    /**
     * Returns the description for the command.
     *
     * @return string The description for the command
     *
     * @api
     */
    public function getDescription()
    {
        return $this->description;
    }

    /**
     * Sets the help for the command.
     *
     * @param string $help The help for the command
     *
     * @return Command The current instance
     *
     * @api
     */
    public function setHelp($help)
    {
        $this->help = $help;

        return $this;
    }

    /**
     * Returns the help for the command.
     *
     * @return string The help for the command
     *
     * @api
     */
    public function getHelp()
    {
        return $this->help;
    }

    /**
     * Returns the processed help for the command replacing the %command.name% and
     * %command.full_name% patterns with the real values dynamically.
     *
     * @return string  The processed help for the command
     */
    public function getProcessedHelp()
    {
        $name = $this->name;

        $placeholders = array(
            '%command.name%',
            '%command.full_name%'
        );
        $replacements = array(
            $name,
            $_SERVER['PHP_SELF'].' '.$name
        );

        return str_replace($placeholders, $replacements, $this->getHelp());
    }

    /**
     * Sets the aliases for the command.
     *
     * @param array $aliases An array of aliases for the command
     *
     * @return Command The current instance
     *
     * @api
     */
    public function setAliases($aliases)
    {
        foreach ($aliases as $alias) {
            $this->validateName($alias);
        }

        $this->aliases = $aliases;

        return $this;
    }

    /**
     * Returns the aliases for the command.
     *
     * @return array An array of aliases for the command
     *
     * @api
     */
    public function getAliases()
    {
        return $this->aliases;
    }

    /**
     * Returns the synopsis for the command.
     *
     * @return string The synopsis
     */
    public function getSynopsis()
    {
        if (null === $this->synopsis) {
            $this->synopsis = trim(sprintf('%s %s', $this->name, $this->definition->getSynopsis()));
        }

        return $this->synopsis;
    }

    /**
     * Gets a helper instance by name.
     *
     * @param string $name The helper name
     *
     * @return mixed The helper value
     *
     * @throws \InvalidArgumentException if the helper is not defined
     *
     * @api
     */
    public function getHelper($name)
    {
        return $this->helperSet->get($name);
    }

    /**
     * Returns a text representation of the command.
     *
     * @return string A string representing the command
     */
    public function asText()
    {
        $messages = array(
            '<comment>Usage:</comment>',
            ' '.$this->getSynopsis(),
            '',
        );

        if ($this->getAliases()) {
            $messages[] = '<comment>Aliases:</comment> <info>'.implode(', ', $this->getAliases()).'</info>';
        }

        $messages[] = $this->getNativeDefinition()->asText();

        if ($help = $this->getProcessedHelp()) {
            $messages[] = '<comment>Help:</comment>';
            $messages[] = ' '.str_replace("\n", "\n ", $help)."\n";
        }

        return implode("\n", $messages);
    }

    /**
     * Returns an XML representation of the command.
     *
     * @param Boolean $asDom Whether to return a DOM or an XML string
     *
     * @return string|DOMDocument An XML string representing the command
     */
    public function asXml($asDom = false)
    {
        $dom = new \DOMDocument('1.0', 'UTF-8');
        $dom->formatOutput = true;
        $dom->appendChild($commandXML = $dom->createElement('command'));
        $commandXML->setAttribute('id', $this->name);
        $commandXML->setAttribute('name', $this->name);

        $commandXML->appendChild($usageXML = $dom->createElement('usage'));
        $usageXML->appendChild($dom->createTextNode(sprintf($this->getSynopsis(), '')));

        $commandXML->appendChild($descriptionXML = $dom->createElement('description'));
        $descriptionXML->appendChild($dom->createTextNode(str_replace("\n", "\n ", $this->getDescription())));

        $commandXML->appendChild($helpXML = $dom->createElement('help'));
        $helpXML->appendChild($dom->createTextNode(str_replace("\n", "\n ", $this->getProcessedHelp())));

        $commandXML->appendChild($aliasesXML = $dom->createElement('aliases'));
        foreach ($this->getAliases() as $alias) {
            $aliasesXML->appendChild($aliasXML = $dom->createElement('alias'));
            $aliasXML->appendChild($dom->createTextNode($alias));
        }

        $definition = $this->getNativeDefinition()->asXml(true);
        $commandXML->appendChild($dom->importNode($definition->getElementsByTagName('arguments')->item(0), true));
        $commandXML->appendChild($dom->importNode($definition->getElementsByTagName('options')->item(0), true));

        return $asDom ? $dom : $dom->saveXml();
    }

    private function validateName($name)
    {
        if (!preg_match('/^[^\:]+(\:[^\:]+)*$/', $name)) {
            throw new \InvalidArgumentException(sprintf('Command name "%s" is invalid.', $name));
        }
    }
}
