<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * An XML util for converting XML to DOMDocument or SimpleXMLElement or to Array.
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Xml
{
  /**
   * Convert anything DOMDocument|SimpleXMLElement|string to DOMDocument.
   *
   * @param \DOMDocument|\SimpleXMLElement|string $xml String may be filename or xml string
   *
   * @throws \InvalidArgumentException
   * @return \DOMDocument
   */
  public static function toDOMDocument($xml)
  {
    if ($xml instanceof \DOMDocument) {
      return $xml;
    }

    if ($xml instanceof \SimpleXMLElement) {
      $doc = new \DOMDocument();
      $doc->loadXML('' . $xml->asXML());

      return $doc;
    }

    if (is_string($xml)) {
      $doc = new \DOMDocument();

      if (is_file($xml)) {
        $doc->load($xml);

        return $doc;
      }

      $doc->loadXML($xml);

      return $doc;
    }

    $type = is_object($xml) ? get_class($xml) : gettype($xml);

    throw new \InvalidArgumentException("Cannot convert instance of '$type' to DOMDocument");
  }

  /**
   * Convert anything DOMDocument|SimpleXMLElement|string to SimpleXMLElement.
   *
   * @param \DOMDocument|\SimpleXMLElement|string $xml String may be filename or xml string
   *
   * @throws \InvalidArgumentException
   * @return \SimpleXMLElement
   */
  public static function toSimpleXMLElement($xml)
  {
    if ($xml instanceof \SimpleXMLElement) {
      return $xml;
    }

    if ($xml instanceof \DOMDocument) {
      return simplexml_import_dom($xml);
    }

    if (is_string($xml)) {

      if (is_file($xml)) {
        return simplexml_load_file($xml);
      }

      return simplexml_load_string($xml);
    }

    $type = is_object($xml) ? get_class($xml) : gettype($xml);

    throw new \InvalidArgumentException("Cannot convert instance of '$type' to DOMDocument");
  }

  /**
   * Convert SimpleXMLElement to multidimensional array.
   *
   * @param \SimpleXMLElement $xml
   * @param string            $namespace The namespace that schould be used.
   *
   * @throws \OutOfBoundsException If namespace not found in the xml.
   * @return array
   */
  public static function toArray(\SimpleXMLElement $xml, $namespace = null)
  {
    if ($namespace !== null) {

      $namespaces = $xml->getNamespaces();

      if (false === isset($namespaces[$namespace])) {
        throw new \OutOfBoundsException('namespace [' . $namespace . '] not found');
      }

      $xml = $xml->children($namespaces[$namespace]);
    }

    return Json::decode(Json::encode($xml), true);
  }
}
