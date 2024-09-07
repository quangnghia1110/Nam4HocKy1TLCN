package studentConsulting.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.ICommonQuestionService;

@RestController
@RequestMapping(value = "/api/v1/common-question")
public class CommonQuestionController {

	@Autowired
	private ICommonQuestionService commonQuestionService;

	@GetMapping("/list")
	public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestions(
	        @RequestParam(required = false) Integer departmentId,
	        @RequestParam(required = false) String title,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "title") String sortBy,
	        @RequestParam(defaultValue = "asc") String sortDir) {

	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
	    Page<CommonQuestionDTO> commonQuestions;

	    // Kiểm tra xem có departmentId và title không để gọi service tương ứng
	    if (departmentId != null && title != null) {
	        commonQuestions = commonQuestionService.getCommonQuestionsByDepartmentAndTitle(departmentId, title, pageable);
	    } else if (departmentId != null) {
	        commonQuestions = commonQuestionService.getCommonQuestionsByDepartment(departmentId, pageable);
	    } else if (title != null) {
	        commonQuestions = commonQuestionService.searchCommonQuestionsByTitle(title, pageable);
	    } else {
	        commonQuestions = commonQuestionService.getAllCommonQuestions(pageable);
	    }

	    if (commonQuestions.isEmpty()) {
	        return ResponseEntity.status(404).body(
	            DataResponse.<Page<CommonQuestionDTO>>builder()
	                .status("error")
	                .message("No common questions found.")
	                .build()
	        );
	    }

	    return ResponseEntity.ok(
	        DataResponse.<Page<CommonQuestionDTO>>builder()
	            .status("success")
	            .message("Fetched common questions successfully.")
	            .data(commonQuestions)
	            .build()
	    );
	}

	
	@PostMapping("/convert-to-common")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> convertToCommonQuestion(@RequestParam Integer questionId) {
        // Gọi dịch vụ để chuyển câu hỏi thành câu hỏi chung
        CommonQuestionDTO commonQuestion = commonQuestionService.convertToCommonQuestion(questionId);

        if (commonQuestion == null) {
            return ResponseEntity.status(404).body(DataResponse.<CommonQuestionDTO>builder()
                    .status("error")
                    .message("Question not found with ID: " + questionId)
                    .build());
        }

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Question converted to common question successfully.")
                .data(commonQuestion)
                .build());
    }
}
