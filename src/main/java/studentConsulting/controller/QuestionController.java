package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.request.main.questionRequest;
import studentConsulting.response.apiResponse;
import studentConsulting.service.implement.questionServiceImpl;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping(value="/api/question")
public class QuestionController {
	@Autowired
	private questionServiceImpl questionService;
	
	@GetMapping(value = "/all")
	public apiResponse<Object> getAllQuestion(){
		return questionService.getAllQuestion();
	}
	
	@PostMapping(value = "/add")
	public apiResponse<Object> addQuestion(@RequestBody questionRequest questionRequest){
		return questionService.addQuestion(questionRequest);
	}
	
	@PutMapping(value = "/update/{id}")
	public apiResponse<Object> updateQuestion(@PathVariable("id") Integer id, @RequestBody questionRequest questionRequest){
		return questionService.updateQuestion(id, questionRequest);
	}
	
	@DeleteMapping(value = "/delete/{id}")
	public apiResponse<Object> deleteQuestion(@PathVariable("id") Integer id){
		return questionService.deleteQuestion(id);
	}
	
}
