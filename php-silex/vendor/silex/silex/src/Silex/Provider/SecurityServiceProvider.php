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

use Symfony\Component\HttpFoundation\RequestMatcher;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\Security\Core\SecurityContext;
use Symfony\Component\Security\Core\SecurityContextInterface;
use Symfony\Component\Security\Core\User\UserChecker;
use Symfony\Component\Security\Core\User\InMemoryUserProvider;
use Symfony\Component\Security\Core\Encoder\EncoderFactory;
use Symfony\Component\Security\Core\Encoder\MessageDigestPasswordEncoder;
use Symfony\Component\Security\Core\Authentication\Provider\DaoAuthenticationProvider;
use Symfony\Component\Security\Core\Authentication\Provider\AnonymousAuthenticationProvider;
use Symfony\Component\Security\Core\Authentication\AuthenticationProviderManager;
use Symfony\Component\Security\Core\Authentication\AuthenticationTrustResolver;
use Symfony\Component\Security\Http\Authentication\DefaultAuthenticationSuccessHandler;
use Symfony\Component\Security\Http\Authentication\DefaultAuthenticationFailureHandler;
use Symfony\Component\Security\Core\Authorization\Voter\RoleHierarchyVoter;
use Symfony\Component\Security\Core\Authorization\Voter\AuthenticatedVoter;
use Symfony\Component\Security\Core\Authorization\AccessDecisionManager;
use Symfony\Component\Security\Core\Role\RoleHierarchy;
use Symfony\Component\Security\Http\Firewall;
use Symfony\Component\Security\Http\FirewallMap;
use Symfony\Component\Security\Http\Firewall\AbstractAuthenticationListener;
use Symfony\Component\Security\Http\Firewall\AccessListener;
use Symfony\Component\Security\Http\Firewall\BasicAuthenticationListener;
use Symfony\Component\Security\Http\Firewall\LogoutListener;
use Symfony\Component\Security\Http\Firewall\SwitchUserListener;
use Symfony\Component\Security\Http\Firewall\AnonymousAuthenticationListener;
use Symfony\Component\Security\Http\Firewall\ContextListener;
use Symfony\Component\Security\Http\Firewall\ExceptionListener;
use Symfony\Component\Security\Http\Firewall\ChannelListener;
use Symfony\Component\Security\Http\EntryPoint\FormAuthenticationEntryPoint;
use Symfony\Component\Security\Http\EntryPoint\BasicAuthenticationEntryPoint;
use Symfony\Component\Security\Http\EntryPoint\RetryAuthenticationEntryPoint;
use Symfony\Component\Security\Http\Session\SessionAuthenticationStrategy;
use Symfony\Component\Security\Http\Logout\SessionLogoutHandler;
use Symfony\Component\Security\Http\Logout\DefaultLogoutSuccessHandler;
use Symfony\Component\Security\Http\AccessMap;
use Symfony\Component\Security\Http\HttpUtils;

/**
 * Symfony Security component Provider.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class SecurityServiceProvider implements ServiceProviderInterface
{
    protected $fakeRoutes;

    public function register(Application $app)
    {
        // used to register routes for login_check and logout
        $this->fakeRoutes = array();

        $that = $this;

        $app['security.role_hierarchy'] = array();
        $app['security.access_rules'] = array();

        $app['security'] = $app->share(function ($app) {
            return new SecurityContext($app['security.authentication_manager'], $app['security.access_manager']);
        });

        $app['security.authentication_manager'] = $app->share(function ($app) {
            $manager = new AuthenticationProviderManager($app['security.authentication_providers']);
            $manager->setEventDispatcher($app['dispatcher']);

            return $manager;
        });

        // by default, all users use the digest encoder
        $app['security.encoder_factory'] = $app->share(function ($app) {
            return new EncoderFactory(array(
                'Symfony\Component\Security\Core\User\UserInterface' => $app['security.encoder.digest'],
            ));
        });

        $app['security.encoder.digest'] = $app->share(function ($app) {
            return new MessageDigestPasswordEncoder();
        });

        $app['security.user_checker'] = $app->share(function ($app) {
            return new UserChecker();
        });

        $app['security.access_manager'] = $app->share(function ($app) {
            return new AccessDecisionManager($app['security.voters']);
        });

        $app['security.voters'] = $app->share(function ($app) {
            return array(
                new RoleHierarchyVoter(new RoleHierarchy($app['security.role_hierarchy'])),
                new AuthenticatedVoter($app['security.trust_resolver']),
            );
        });

        $app['security.firewall'] = $app->share(function ($app) {
            return new Firewall($app['security.firewall_map'], $app['dispatcher']);
        });

        $app['security.channel_listener'] = $app->share(function ($app) {
            return new ChannelListener(
                $app['security.access_map'],
                new RetryAuthenticationEntryPoint($app['request.http_port'], $app['request.https_port']),
                $app['logger']
            );
        });

        // generate the build-in authentication factories
        foreach (array('logout', 'pre_auth', 'form', 'http', 'remember_me', 'anonymous') as $type) {
            $entryPoint = null;
            if ('http' === $type) {
                $entryPoint = 'http';
            } elseif ('form' === $type) {
                $entryPoint = 'form';
            }

            $app['security.authentication_listener.factory.'.$type] = $app->protect(function($name, $options) use ($type, $app, $entryPoint) {
                if ($entryPoint && !isset($app['security.entry_point.'.$name.'.'.$entryPoint])) {
                    $app['security.entry_point.'.$name.'.'.$entryPoint] = $app['security.entry_point.'.$entryPoint.'._proto']($name, $options);
                }

                if (!isset($app['security.authentication_listener.'.$name.'.'.$type])) {
                    $app['security.authentication_listener.'.$name.'.'.$type] = $app['security.authentication_listener.'.$type.'._proto']($name, $options);
                }

                $provider = 'anonymous' === $type ? 'anonymous' : 'dao';
                if (!isset($app['security.authentication_provider.'.$name.'.'.$provider])) {
                    $app['security.authentication_provider.'.$name.'.'.$provider] = $app['security.authentication_provider.'.$provider.'._proto']($name);
                }

                return array(
                    'security.authentication_provider.'.$name.'.'.$provider,
                    'security.authentication_listener.'.$name.'.'.$type,
                    $entryPoint ? 'security.entry_point.'.$name.'.'.$entryPoint : null,
                    $type
                );
            });
        }

        $app['security.firewall_map'] = $app->share(function ($app) {
            $positions = array('logout', 'pre_auth', 'form', 'http', 'remember_me', 'anonymous');
            $providers = array();
            $configs = array();
            foreach ($app['security.firewalls'] as $name => $firewall) {
                $entryPoint = null;
                $pattern = isset($firewall['pattern']) ? $firewall['pattern'] : null;
                $users = isset($firewall['users']) ? $firewall['users'] : array();
                $security = isset($firewall['security']) ? (Boolean) $firewall['security'] : true;
                $stateless = isset($firewall['stateless']) ? (Boolean) $firewall['stateless'] : false;
                unset($firewall['pattern'], $firewall['users'], $firewall['security'], $firewall['stateless']);

                $protected = false === $security ? false : count($firewall);

                $listeners = array('security.channel_listener');

                if ($protected) {
                    if (!isset($app['security.context_listener.'.$name])) {
                        if (!isset($app['security.user_provider.'.$name])) {
                            $app['security.user_provider.'.$name] = is_array($users) ? $app['security.user_provider.inmemory._proto']($users) : $users;
                        }

                        $app['security.context_listener.'.$name] = $app['security.context_listener._proto']($name, array($app['security.user_provider.'.$name]));
                    }

                    if (false === $stateless) {
                        $listeners[] = 'security.context_listener.'.$name;
                    }

                    $factories = array();
                    foreach ($positions as $position) {
                        $factories[$position] = array();
                    }

                    foreach ($firewall as $type => $options) {
                        if ('switch_user' === $type) {
                            continue;
                        }

                        // normalize options
                        if (!is_array($options)) {
                            if (!$options) {
                                continue;
                            }

                            $options = array();
                        }

                        if (!isset($app['security.authentication_listener.factory.'.$type])) {
                            throw new \LogicException(sprintf('The "%s" authentication entry is not registered.', $type));
                        }

                        list($providerId, $listenerId, $entryPointId, $position) = $app['security.authentication_listener.factory.'.$type]($name, $options);

                        if (null !== $entryPointId) {
                            $entryPoint = $entryPointId;
                        }

                        $factories[$position][] = $listenerId;
                        $providers[] = $providerId;
                    }

                    foreach ($positions as $position) {
                        foreach ($factories[$position] as $listener) {
                            $listeners[] = $listener;
                        }
                    }

                    $listeners[] = 'security.access_listener';

                    if (isset($firewall['switch_user'])) {
                        $app['security.switch_user.'.$name] = $app['security.authentication_listener.switch_user._proto']($name, $firewall['switch_user']);

                        $listeners[] = 'security.switch_user.'.$name;
                    }

                    if (!isset($app['security.exception_listener.'.$name])) {
                        if (null == $entryPoint) {
                            $app[$entryPoint = 'security.entry_point.'.$name.'.form'] = $app['security.entry_point.form._proto']($name, array());
                        }
                        $app['security.exception_listener.'.$name] = $app['security.exception_listener._proto']($entryPoint, $name);
                    }
                }

                $configs[$name] = array($pattern, $listeners, $protected);
            }

            $app['security.authentication_providers'] = array_map(function ($provider) use ($app) {
                return $app[$provider];
            }, $providers);

            $map = new FirewallMap();
            foreach ($configs as $name => $config) {
                $map->add(
                    is_string($config[0]) ? new RequestMatcher($config[0]) : $config[0],
                    array_map(function ($listenerId) use ($app, $name) {
                        $listener = $app[$listenerId];

                        if (isset($app['security.remember_me.service.'.$name])) {
                            if ($listener instanceof AbstractAuthenticationListener) {
                                $listener->setRememberMeServices($app['security.remember_me.service.'.$name]);
                            }
                            if ($listener instanceof LogoutListener) {
                                $listener->addHandler($app['security.remember_me.service.'.$name]);
                            }
                        }

                        return $listener;
                    }, $config[1]),
                    $config[2] ? $app['security.exception_listener.'.$name] : null
                );
            }

            return $map;
        });

        $app['security.access_listener'] = $app->share(function ($app) {
            return new AccessListener(
                $app['security'],
                $app['security.access_manager'],
                $app['security.access_map'],
                $app['security.authentication_manager'],
                $app['logger']
            );
        });

        $app['security.access_map'] = $app->share(function ($app) {
            $map = new AccessMap();

            foreach ($app['security.access_rules'] as $rule) {
                if (is_string($rule[0])) {
                    $rule[0] = new RequestMatcher($rule[0]);
                }

                $map->add($rule[0], (array) $rule[1], isset($rule[2]) ? $rule[2] : null);
            }

            return $map;
        });

        $app['security.trust_resolver'] = $app->share(function ($app) {
            return new AuthenticationTrustResolver('Symfony\Component\Security\Core\Authentication\Token\AnonymousToken', 'Symfony\Component\Security\Core\Authentication\Token\RememberMeToken');
        });

        $app['security.session_strategy'] = $app->share(function ($app) {
            return new SessionAuthenticationStrategy('migrate');
        });

        $app['security.http_utils'] = $app->share(function ($app) {
            return new HttpUtils(isset($app['url_generator']) ? $app['url_generator'] : null, $app['url_matcher']);
        });

        $app['security.last_error'] = $app->protect(function (Request $request) {
            if ($request->attributes->has(SecurityContextInterface::AUTHENTICATION_ERROR)) {
                return $request->attributes->get(SecurityContextInterface::AUTHENTICATION_ERROR)->getMessage();
            }

            $session = $request->getSession();
            if ($session && $session->has(SecurityContextInterface::AUTHENTICATION_ERROR)) {
                $error = $session->get(SecurityContextInterface::AUTHENTICATION_ERROR)->getMessage();
                $session->remove(SecurityContextInterface::AUTHENTICATION_ERROR);

                return $error;
            }
        });

        // prototypes (used by the Firewall Map)

        $app['security.context_listener._proto'] = $app->protect(function ($providerKey, $userProviders) use ($app) {
            return $app->share(function () use ($app, $userProviders, $providerKey) {
                return new ContextListener(
                    $app['security'],
                    $userProviders,
                    $providerKey,
                    $app['logger'],
                    $app['dispatcher']
                );
            });
        });

        $app['security.user_provider.inmemory._proto'] = $app->protect(function ($params) use ($app) {
            return $app->share(function () use ($app, $params) {
                $users = array();
                foreach ($params as $name => $user) {
                    $users[$name] = array('roles' => (array) $user[0], 'password' => $user[1]);
                }

                return new InMemoryUserProvider($users);
            });
        });

        $app['security.exception_listener._proto'] = $app->protect(function ($entryPoint, $name) use ($app) {
            return $app->share(function () use ($app, $entryPoint, $name) {
                return new ExceptionListener(
                    $app['security'],
                    $app['security.trust_resolver'],
                    $app['security.http_utils'],
                    $name,
                    $app[$entryPoint],
                    null, // errorPage
                    null, // AccessDeniedHandlerInterface
                    $app['logger']
                );
            });
        });

        $app['security.authentication.success_handler._proto'] = $app->protect(function ($name, $options) use ($app) {
            return $app->share(function () use ($name, $options, $app) {
                $handler = new DefaultAuthenticationSuccessHandler(
                    $app['security.http_utils'],
                    $options
                );
                $handler->setProviderKey($name);

                return $handler;
            });
        });

        $app['security.authentication.failure_handler._proto'] = $app->protect(function ($name, $options) use ($app) {
            return $app->share(function () use ($name, $options, $app) {
                return new DefaultAuthenticationFailureHandler(
                    $app,
                    $app['security.http_utils'],
                    $options,
                    $app['logger']
                );
            });
        });

        $app['security.authentication_listener.form._proto'] = $app->protect(function ($name, $options) use ($app, $that) {
            return $app->share(function () use ($app, $name, $options, $that) {
                $that->addFakeRoute(
                    'match',
                    $tmp = isset($options['check_path']) ? $options['check_path'] : '/login_check',
                    str_replace('/', '_', ltrim($tmp, '/'))
                );

                $class = isset($options['listener_class']) ? $options['listener_class'] : 'Symfony\\Component\\Security\\Http\\Firewall\\UsernamePasswordFormAuthenticationListener';

                if (!isset($app['security.authentication.success_handler.'.$name])) {
                    $app['security.authentication.success_handler.'.$name] = $app['security.authentication.success_handler._proto']($name, $options);
                }

                if (!isset($app['security.authentication.failure_handler.'.$name])) {
                    $app['security.authentication.failure_handler.'.$name] = $app['security.authentication.failure_handler._proto']($name, $options);
                }

                return new $class(
                    $app['security'],
                    $app['security.authentication_manager'],
                    isset($app['security.session_strategy.'.$name]) ? $app['security.session_strategy.'.$name] : $app['security.session_strategy'],
                    $app['security.http_utils'],
                    $name,
                    $app['security.authentication.success_handler.'.$name],
                    $app['security.authentication.failure_handler.'.$name],
                    $options,
                    $app['logger'],
                    $app['dispatcher'],
                    isset($options['with_csrf']) && $options['with_csrf'] && isset($app['form.csrf_provider']) ? $app['form.csrf_provider'] : null
                );
            });
        });

        $app['security.authentication_listener.http._proto'] = $app->protect(function ($providerKey, $options) use ($app) {
            return $app->share(function () use ($app, $providerKey, $options) {
                return new BasicAuthenticationListener(
                    $app['security'],
                    $app['security.authentication_manager'],
                    $providerKey,
                    $app['security.entry_point.'.$providerKey.'.http'],
                    $app['logger']
                );
            });
        });

        $app['security.authentication_listener.anonymous._proto'] = $app->protect(function ($providerKey, $options) use ($app) {
            return $app->share(function () use ($app, $providerKey, $options) {
                return new AnonymousAuthenticationListener(
                    $app['security'],
                    $providerKey,
                    $app['logger']
                );
            });
        });

        $app['security.authentication.logout_handler._proto'] = $app->protect(function ($name, $options) use ($app) {
            return $app->share(function () use ($name, $options, $app) {
                return new DefaultLogoutSuccessHandler(
                    $app['security.http_utils'],
                    isset($options['target_url']) ? $options['target_url'] : '/'
                );
            });
        });

        $app['security.authentication_listener.logout._proto'] = $app->protect(function ($name, $options) use ($app, $that) {
            return $app->share(function () use ($app, $name, $options, $that) {
                $that->addFakeRoute(
                    'get',
                    $tmp = isset($options['logout_path']) ? $options['logout_path'] : '/logout',
                    str_replace('/', '_', ltrim($tmp, '/'))
                );

                if (!isset($app['security.authentication.logout_handler.'.$name])) {
                    $app['security.authentication.logout_handler.'.$name] = $app['security.authentication.logout_handler._proto']($name, $options);
                }

                $listener = new LogoutListener(
                    $app['security'],
                    $app['security.http_utils'],
                    $app['security.authentication.logout_handler.'.$name],
                    $options,
                    isset($options['with_csrf']) && $options['with_csrf'] && isset($app['form.csrf_provider']) ? $app['form.csrf_provider'] : null
                );

                $listener->addHandler(new SessionLogoutHandler());

                return $listener;
            });
        });

        $app['security.authentication_listener.switch_user._proto'] = $app->protect(function ($name, $options) use ($app, $that) {
            return $app->share(function () use ($app, $name, $options, $that) {
                return new SwitchUserListener(
                    $app['security'],
                    $app['security.user_provider.'.$name],
                    $app['security.user_checker'],
                    $name,
                    $app['security.access_manager'],
                    $app['logger'],
                    isset($options['parameter']) ? $options['parameter'] : '_switch_user',
                    isset($options['role']) ? $options['role'] : 'ROLE_ALLOWED_TO_SWITCH',
                    $app['dispatcher']
                );
            });
        });

        $app['security.entry_point.form._proto'] = $app->protect(function ($name, array $options) use ($app) {
            return $app->share(function () use ($app, $options) {
                $loginPath = isset($options['login_path']) ? $options['login_path'] : '/login';
                $useForward = isset($options['use_forward']) ? $options['use_forward'] : false;

                return new FormAuthenticationEntryPoint($app, $app['security.http_utils'], $loginPath, $useForward);
            });
        });

        $app['security.entry_point.http._proto'] = $app->protect(function ($name, array $options) use ($app) {
            return $app->share(function () use ($app, $name, $options) {
                return new BasicAuthenticationEntryPoint(isset($options['real_name']) ? $options['real_name'] : 'Secured');
            });
        });

        $app['security.authentication_provider.dao._proto'] = $app->protect(function ($name) use ($app) {
            return $app->share(function () use ($app, $name) {
                return new DaoAuthenticationProvider(
                    $app['security.user_provider.'.$name],
                    $app['security.user_checker'],
                    $name,
                    $app['security.encoder_factory']
                );
            });
        });

        $app['security.authentication_provider.anonymous._proto'] = $app->protect(function ($name) use ($app) {
            return $app->share(function () use ($app, $name) {
                return new AnonymousAuthenticationProvider($name);
            });
        });
    }

    public function boot(Application $app)
    {
        // FIXME: in Symfony 2.2, this is a proper subscriber
        //$app['dispatcher']->addSubscriber($app['security.firewall']);
        $app['dispatcher']->addListener('kernel.request', array($app['security.firewall'], 'onKernelRequest'), 8);

        foreach ($this->fakeRoutes as $route) {
            list($method, $pattern, $name) = $route;

            $app->$method($pattern, function() {})->bind($name);
        }
    }

    public function addFakeRoute($method, $pattern, $name)
    {
        $this->fakeRoutes[] = array($method, $pattern, $name);
    }
}
