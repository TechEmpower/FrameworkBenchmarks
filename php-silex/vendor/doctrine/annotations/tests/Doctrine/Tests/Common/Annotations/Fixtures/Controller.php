<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Template;
use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Route;

/**
 * @Route("/someprefix")
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 */
class Controller
{
    /**
     * @Route("/", name="_demo")
     * @Template()
     */
    public function indexAction()
    {
        return array();
    }

    /**
     * @Route("/hello/{name}", name="_demo_hello")
     * @Template()
     */
    public function helloAction($name)
    {
        return array('name' => $name);
    }

    /**
     * @Route("/contact", name="_demo_contact")
     * @Template()
     */
    public function contactAction()
    {
        $form = ContactForm::create($this->get('form.context'), 'contact');

        $form->bind($this->container->get('request'), $form);
        if ($form->isValid()) {
            $form->send($this->get('mailer'));

            $this->get('session')->setFlash('notice', 'Message sent!');

            return new RedirectResponse($this->generateUrl('_demo'));
        }

        return array('form' => $form);
    }

    /**
     * Creates the ACL for the passed object identity
     *
     * @param ObjectIdentityInterface $oid
     * @return void
     */
    private function createObjectIdentity(ObjectIdentityInterface $oid)
    {
        $classId = $this->createOrRetrieveClassId($oid->getType());

        $this->connection->executeQuery($this->getInsertObjectIdentitySql($oid->getIdentifier(), $classId, true));
    }

    /**
     * Returns the primary key for the passed class type.
     *
     * If the type does not yet exist in the database, it will be created.
     *
     * @param string $classType
     * @return integer
     */
    private function createOrRetrieveClassId($classType)
    {
        if (false !== $id = $this->connection->executeQuery($this->getSelectClassIdSql($classType))->fetchColumn()) {
            return $id;
        }

        $this->connection->executeQuery($this->getInsertClassSql($classType));

        return $this->connection->executeQuery($this->getSelectClassIdSql($classType))->fetchColumn();
    }

    /**
     * Returns the primary key for the passed security identity.
     *
     * If the security identity does not yet exist in the database, it will be
     * created.
     *
     * @param SecurityIdentityInterface $sid
     * @return integer
     */
    private function createOrRetrieveSecurityIdentityId(SecurityIdentityInterface $sid)
    {
        if (false !== $id = $this->connection->executeQuery($this->getSelectSecurityIdentityIdSql($sid))->fetchColumn()) {
            return $id;
        }

        $this->connection->executeQuery($this->getInsertSecurityIdentitySql($sid));

        return $this->connection->executeQuery($this->getSelectSecurityIdentityIdSql($sid))->fetchColumn();
    }

    /**
     * Deletes all ACEs for the given object identity primary key.
     *
     * @param integer $oidPK
     * @return void
     */
    private function deleteAccessControlEntries($oidPK)
    {
        $this->connection->executeQuery($this->getDeleteAccessControlEntriesSql($oidPK));
    }

    /**
     * Deletes the object identity from the database.
     *
     * @param integer $pk
     * @return void
     */
    private function deleteObjectIdentity($pk)
    {
        $this->connection->executeQuery($this->getDeleteObjectIdentitySql($pk));
    }

    /**
     * Deletes all entries from the relations table from the database.
     *
     * @param integer $pk
     * @return void
     */
    private function deleteObjectIdentityRelations($pk)
    {
        $this->connection->executeQuery($this->getDeleteObjectIdentityRelationsSql($pk));
    }

    /**
     * This regenerates the ancestor table which is used for fast read access.
     *
     * @param AclInterface $acl
     * @return void
     */
    private function regenerateAncestorRelations(AclInterface $acl)
    {
        $pk = $acl->getId();
        $this->connection->executeQuery($this->getDeleteObjectIdentityRelationsSql($pk));
        $this->connection->executeQuery($this->getInsertObjectIdentityRelationSql($pk, $pk));

        $parentAcl = $acl->getParentAcl();
        while (null !== $parentAcl) {
            $this->connection->executeQuery($this->getInsertObjectIdentityRelationSql($pk, $parentAcl->getId()));

            $parentAcl = $parentAcl->getParentAcl();
        }
    }

    /**
     * This processes changes on an ACE related property (classFieldAces, or objectFieldAces).
     *
     * @param string $name
     * @param array $changes
     * @return void
     */
    private function updateFieldAceProperty($name, array $changes)
    {
        $sids = new \SplObjectStorage();
        $classIds = new \SplObjectStorage();
        $currentIds = array();
        foreach ($changes[1] as $field => $new) {
            for ($i=0,$c=count($new); $i<$c; $i++) {
                $ace = $new[$i];

                if (null === $ace->getId()) {
                    if ($sids->contains($ace->getSecurityIdentity())) {
                        $sid = $sids->offsetGet($ace->getSecurityIdentity());
                    } else {
                        $sid = $this->createOrRetrieveSecurityIdentityId($ace->getSecurityIdentity());
                    }

                    $oid = $ace->getAcl()->getObjectIdentity();
                    if ($classIds->contains($oid)) {
                        $classId = $classIds->offsetGet($oid);
                    } else {
                        $classId = $this->createOrRetrieveClassId($oid->getType());
                    }

                    $objectIdentityId = $name === 'classFieldAces' ? null : $ace->getAcl()->getId();

                    $this->connection->executeQuery($this->getInsertAccessControlEntrySql($classId, $objectIdentityId, $field, $i, $sid, $ace->getStrategy(), $ace->getMask(), $ace->isGranting(), $ace->isAuditSuccess(), $ace->isAuditFailure()));
                    $aceId = $this->connection->executeQuery($this->getSelectAccessControlEntryIdSql($classId, $objectIdentityId, $field, $i))->fetchColumn();
                    $this->loadedAces[$aceId] = $ace;

                    $aceIdProperty = new \ReflectionProperty('Symfony\Component\Security\Acl\Domain\Entry', 'id');
                    $aceIdProperty->setAccessible(true);
                    $aceIdProperty->setValue($ace, intval($aceId));
                } else {
                    $currentIds[$ace->getId()] = true;
                }
            }
        }

        foreach ($changes[0] as $old) {
            for ($i=0,$c=count($old); $i<$c; $i++) {
                $ace = $old[$i];

                if (!isset($currentIds[$ace->getId()])) {
                    $this->connection->executeQuery($this->getDeleteAccessControlEntrySql($ace->getId()));
                    unset($this->loadedAces[$ace->getId()]);
                }
            }
        }
    }

    /**
     * This processes changes on an ACE related property (classAces, or objectAces).
     *
     * @param string $name
     * @param array $changes
     * @return void
     */
    private function updateAceProperty($name, array $changes)
    {
        list($old, $new) = $changes;

        $sids = new \SplObjectStorage();
        $classIds = new \SplObjectStorage();
        $currentIds = array();
        for ($i=0,$c=count($new); $i<$c; $i++) {
            $ace = $new[$i];

            if (null === $ace->getId()) {
                if ($sids->contains($ace->getSecurityIdentity())) {
                    $sid = $sids->offsetGet($ace->getSecurityIdentity());
                } else {
                    $sid = $this->createOrRetrieveSecurityIdentityId($ace->getSecurityIdentity());
                }

                $oid = $ace->getAcl()->getObjectIdentity();
                if ($classIds->contains($oid)) {
                    $classId = $classIds->offsetGet($oid);
                } else {
                    $classId = $this->createOrRetrieveClassId($oid->getType());
                }

                $objectIdentityId = $name === 'classAces' ? null : $ace->getAcl()->getId();

                $this->connection->executeQuery($this->getInsertAccessControlEntrySql($classId, $objectIdentityId, null, $i, $sid, $ace->getStrategy(), $ace->getMask(), $ace->isGranting(), $ace->isAuditSuccess(), $ace->isAuditFailure()));
                $aceId = $this->connection->executeQuery($this->getSelectAccessControlEntryIdSql($classId, $objectIdentityId, null, $i))->fetchColumn();
                $this->loadedAces[$aceId] = $ace;

                $aceIdProperty = new \ReflectionProperty($ace, 'id');
                $aceIdProperty->setAccessible(true);
                $aceIdProperty->setValue($ace, intval($aceId));
            } else {
                $currentIds[$ace->getId()] = true;
            }
        }

        for ($i=0,$c=count($old); $i<$c; $i++) {
            $ace = $old[$i];

            if (!isset($currentIds[$ace->getId()])) {
                $this->connection->executeQuery($this->getDeleteAccessControlEntrySql($ace->getId()));
                unset($this->loadedAces[$ace->getId()]);
            }
        }
    }

    /**
     * Persists the changes which were made to ACEs to the database.
     *
     * @param \SplObjectStorage $aces
     * @return void
     */
    private function updateAces(\SplObjectStorage $aces)
    {
        foreach ($aces as $ace) {
            $propertyChanges = $aces->offsetGet($ace);
            $sets = array();

            if (isset($propertyChanges['mask'])) {
                $sets[] = sprintf('mask = %d', $propertyChanges['mask'][1]);
            }
            if (isset($propertyChanges['strategy'])) {
                $sets[] = sprintf('granting_strategy = %s', $this->connection->quote($propertyChanges['strategy']));
            }
            if (isset($propertyChanges['aceOrder'])) {
                $sets[] = sprintf('ace_order = %d', $propertyChanges['aceOrder'][1]);
            }
            if (isset($propertyChanges['auditSuccess'])) {
                $sets[] = sprintf('audit_success = %s', $this->connection->getDatabasePlatform()->convertBooleans($propertyChanges['auditSuccess'][1]));
            }
            if (isset($propertyChanges['auditFailure'])) {
                $sets[] = sprintf('audit_failure = %s', $this->connection->getDatabasePlatform()->convertBooleans($propertyChanges['auditFailure'][1]));
            }

            $this->connection->executeQuery($this->getUpdateAccessControlEntrySql($ace->getId(), $sets));
        }
    }
}