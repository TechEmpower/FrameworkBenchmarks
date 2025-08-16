<?php

declare(strict_types=1);

namespace App\Http\Requests;

use Hypervel\Foundation\Http\FormRequest;

class DemoRequest extends FormRequest
{
    /**
     * Determine if the user is authorized to make this request.
     */
    public function authorize(): bool
    {
        return true;
    }

    /**
     * Get the validation rules that apply to the request.
     */
    public function rules(): array
    {
        return [
            'name' => 'required|string|min:3',
            'email' => 'required|email',
            'email2' => 'required|array',
        ];
    }
}
