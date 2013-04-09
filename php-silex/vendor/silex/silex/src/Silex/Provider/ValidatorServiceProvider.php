<?php

/*
 * This file is part of the Silex framework.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Silex\Provider;

use Silex\Application;
use Silex\ServiceProviderInterface;

use Symfony\Component\Validator\Validator;
use Symfony\Component\Validator\DefaultTranslator;
use Symfony\Component\Validator\Mapping\ClassMetadataFactory;
use Symfony\Component\Validator\Mapping\Loader\StaticMethodLoader;
use Symfony\Component\Validator\ConstraintValidatorFactory;

/**
 * Symfony Validator component Provider.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class ValidatorServiceProvider implements ServiceProviderInterface
{
    public function register(Application $app)
    {
        $app['validator'] = $app->share(function ($app) {
            $r = new \ReflectionClass('Symfony\Component\Validator\Validator');

            if (isset($app['translator'])) {
                $app['translator']->addResource('xliff', dirname($r->getFilename()).'/Resources/translations/validators.'.$app['locale'].'.xlf', $app['locale'], 'validators');
            }

            $params = $r->getConstructor()->getParameters();
            if ('validatorInitializers' === $params[2]->getName()) {
                // BC: to be removed before 1.0
                // Compatibility with symfony/validator 2.1
                // can be removed once silex requires 2.2
                return new Validator(
                    $app['validator.mapping.class_metadata_factory'],
                    $app['validator.validator_factory']
                );
            } else {
                return new Validator(
                    $app['validator.mapping.class_metadata_factory'],
                    $app['validator.validator_factory'],
                    isset($app['translator']) ? $app['translator'] : new DefaultTranslator()
                );
            }
        });

        $app['validator.mapping.class_metadata_factory'] = $app->share(function ($app) {
            return new ClassMetadataFactory(new StaticMethodLoader());
        });

        $app['validator.validator_factory'] = $app->share(function () {
            return new ConstraintValidatorFactory();
        });
    }

    public function boot(Application $app)
    {
    }
}
