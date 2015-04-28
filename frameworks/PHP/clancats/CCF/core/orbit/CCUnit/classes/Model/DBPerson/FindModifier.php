<?php namespace CCUnit;
/**
 * CCUnit Ship
 *
 * @package       CCUnit
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 */
class Model_DBPerson_FindModifier extends \DB\Model
{
    protected static $_find_modifier = array( "CCUnit\\Model_DBPerson_FindModifier", 'order_age' );
    
    protected static $_table = 'people';

    /**
     * Order by age
     */
    public function order_age( $q )
    {
        $q->order_by( 'age' );
    }
}
