package studentConsulting.service.interfaces;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

import studentConsulting.request.main.questionRequest;
import studentConsulting.response.apiResponse;

public interface questionServiceInterface {
	public apiResponse<Object> getAllQuestion();
	public apiResponse<Object> addQuestion(@RequestBody questionRequest questionRequest);
	public apiResponse<Object> updateQuestion(@PathVariable("id") Long id, @RequestBody questionRequest questionRequest);
	public apiResponse<Object> deleteQuestion(@PathVariable("id") Long id);
}
