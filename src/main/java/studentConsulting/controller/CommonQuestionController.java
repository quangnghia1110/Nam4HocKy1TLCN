package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.ICommonQuestionService;

@RestController
@RequestMapping("${base.url}")
public class CommonQuestionController {

	@Autowired
	private ICommonQuestionService commonQuestionService;
    
	@Autowired
	private UserRepository userRepository;
	
	@GetMapping("/list-common-question")
	public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestions(
	        @RequestParam(required = false) Integer departmentId,
	        @RequestParam(required = false) String title,
	        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
	        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "title") String sortBy,
	        @RequestParam(defaultValue = "asc") String sortDir) {

	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(departmentId, title, startDate, endDate, pageable);

	    if (commonQuestions.isEmpty()) {
	        return ResponseEntity.status(404).body(
	            DataResponse.<Page<CommonQuestionDTO>>builder()
	                .status("error")
	                .message("Câu hỏi chung không tìm thấy")
	                .build()
	        );
	    }

	    return ResponseEntity.ok(
	        DataResponse.<Page<CommonQuestionDTO>>builder()
	            .status("success")
	            .message("Lấy câu hỏi chung thành công")
	            .data(commonQuestions)
	            .build()
	    );
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@PostMapping("/advisor/common-question/convert-to-common")
	public ResponseEntity<DataResponse<CommonQuestionDTO>> convertToCommonQuestion(@RequestParam Integer questionId, Principal principal) {
	    String email = principal.getName();System.out.println("Email: " + email);
	    Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);if (!userOpt.isPresent()) {throw new ErrorException("Không tìm thấy người dùng");}
	    CommonQuestionDTO commonQuestion = commonQuestionService.convertToCommonQuestion(questionId);

	    if (commonQuestion == null) {
	        throw new ErrorException("Không tìm thấy câu hỏi với ID: " + questionId);
	    }

	    return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
	            .status("success")
	            .message("Chuyển đổi câu hỏi thành công.")
	            .data(commonQuestion)
	            .build());
	}
}