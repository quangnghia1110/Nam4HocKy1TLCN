package studentConsulting.controller;

import java.security.Principal;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.QuestionStatusDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IQuestionService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("/api/v1/question")
public class QuestionController {

	@Autowired
	private IQuestionService questionService;

	@Autowired
	private IUserService userService;

	@GetMapping("/list")
    public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> getAllQuestions(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<MyQuestionDTO> questions = questionService.getAllQuestions(pageable);

        if (questions.isEmpty()) {
            return ResponseEntity.status(404).body(
                DataResponse.<Page<MyQuestionDTO>>builder()
                    .status("error")
                    .message("No common questions found.")
                    .build()
            );
        }

        return ResponseEntity.ok(
            DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message("Fetched all common questions successfully.")
                .data(questions)
                .build()
        );
    }
	@PostMapping(value = "/create", consumes = { "multipart/form-data" })
	public ResponseEntity<DataResponse<QuestionDTO>> createQuestion(@RequestParam("departmentId") Integer departmentId,
			@RequestParam("fieldId") Integer fieldId, @RequestParam("roleAskId") Integer roleAskId,
			@RequestParam("title") String title, @RequestParam("content") String content,
			@RequestParam("firstName") String firstName, @RequestParam("lastName") String lastName,
			@RequestParam("studentCode") String studentCode, @RequestParam("statusPublic") Boolean statusPublic,
			@RequestPart("file") MultipartFile file) {

		// Tạo đối tượng request mà không cần parentQuestionId
		CreateQuestionRequest questionRequest = CreateQuestionRequest.builder().departmentId(departmentId)
				.fieldId(fieldId).roleAskId(roleAskId).title(title).content(content).firstName(firstName)
				.lastName(lastName).studentCode(studentCode).statusPublic(statusPublic).file(file).build();

		// Gọi service để tạo câu hỏi
		return ResponseEntity.ok(questionService.createQuestion(questionRequest));
	}

	@PostMapping(value = "/update", consumes = { "multipart/form-data" })
	public ResponseEntity<DataResponse<QuestionDTO>> updateQuestion(@RequestParam("questionId") Integer questionId,
			@RequestParam("departmentId") Integer departmentId, @RequestParam("fieldId") Integer fieldId,
			@RequestParam("roleAskId") Integer roleAskId, @RequestParam("title") String title,
			@RequestParam("content") String content, @RequestParam("firstName") String firstName,
			@RequestParam("lastName") String lastName, @RequestParam("studentCode") String studentCode,
			@RequestParam("statusPublic") Boolean statusPublic,
			@RequestPart(value = "file", required = false) MultipartFile file) {

		UpdateQuestionRequest questionRequest = UpdateQuestionRequest.builder().departmentId(departmentId)
				.fieldId(fieldId).roleAskId(roleAskId).title(title).content(content).firstName(firstName)
				.lastName(lastName).studentCode(studentCode).statusPublic(statusPublic).file(file).build();

		return ResponseEntity.ok(questionService.updateQuestion(questionId, questionRequest));
	}

	@DeleteMapping("/delete/{id}")
	public ResponseEntity<DataResponse<Void>> deleteQuestion(@PathVariable("id") Integer questionId) {
		return ResponseEntity.ok(questionService.deleteQuestion(questionId));
	}

	@GetMapping("/roleAsk")
	public ResponseEntity<DataResponse<List<RoleAskDTO>>> getAllRoleAsk() {
		List<RoleAskDTO> roleAsks = questionService.getAllRoleAsk();
		DataResponse<List<RoleAskDTO>> response = DataResponse.<List<RoleAskDTO>>builder().status("success")
				.message("Fetched all role ask successfully.").data(roleAsks).build();

		return ResponseEntity.ok(response);
	}

	@PostMapping(value = "/create-follow-up", consumes = { "multipart/form-data" })
	public ResponseEntity<DataResponse<QuestionDTO>> askFollowUpQuestion(
			@RequestParam("parentQuestionId") Integer parentQuestionId, // Nhận parentQuestionId
			@RequestParam("title") String title, @RequestParam("content") String content,
			@RequestPart(value = "file", required = false) MultipartFile file) {

		// Gọi service để tạo câu hỏi follow-up
		return ResponseEntity.ok(questionService.askFollowUpQuestion(parentQuestionId, title, content, file));
	}

	@GetMapping("/user")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> getQuestionsByUser(
	        Principal principal,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();
	    Integer userId = userService.getUserIdByUsername(username);

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Gọi service để lấy danh sách câu hỏi của người dùng với phân trang
	    Page<MyQuestionDTO> questions = questionService.getQuestionsByUserId(userId, pageable);

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error").message("No questions found for user ID: " + userId).build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi của người dùng có phân trang
	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder().status("success")
	            .message("Fetched questions and answers for user successfully.").data(questions).build();

	    return ResponseEntity.ok(response);
	}

	@GetMapping("/search")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> searchQuestionsByTitle(
	        @RequestParam("title") String title,
	        Principal principal,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();
	    Integer userId = userService.getUserIdByUsername(username);

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Gọi service để tìm kiếm câu hỏi theo userId và tiêu đề
	    Page<MyQuestionDTO> questions = questionService.searchQuestionsByTitle(userId, title, pageable);

	    if (questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error").message("No questions found with title: " + title).build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder().status("success")
	            .message("Found questions successfully.").data(questions).build();

	    return ResponseEntity.ok(response);
	}

	@GetMapping("/filter-by-department/{departmentId}")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> filterMyQuestionsByDepartment(
	        @PathVariable("departmentId") Integer departmentId,
	        Principal principal,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();
	    Integer userId = userService.getUserIdByUsername(username);

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Gọi service để lọc câu hỏi theo userId và departmentId
	    Page<MyQuestionDTO> questions = questionService.filterMyQuestionsByDepartment(userId, departmentId, pageable);

	    if (questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error").message("No questions found for the given department.").build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder().status("success")
	            .message("Filtered questions by department successfully.").data(questions).build();

	    return ResponseEntity.ok(response);
	}

	@GetMapping("/filter-all-by-department/{departmentId}")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> filterAllQuestionsByDepartment(
	        @PathVariable("departmentId") Integer departmentId,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {
		
	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Gọi service để lọc câu hỏi theo userId và departmentId
	    Page<MyQuestionDTO> questions = questionService.filterAllQuestionsByDepartment(departmentId, pageable);

	    if (questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error").message("No questions found for the given department.").build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder().status("success")
	            .message("Filtered questions by department successfully.").data(questions).build();

	    return ResponseEntity.ok(response);
	}

	// Lọc câu hỏi theo trạng thái (status) (sử dụng PathVariable)
	@GetMapping("/filter-by-status")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> filterQuestionsByStatus(
	        @RequestParam("status") String status,
	        Principal principal,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();
	    Integer userId = userService.getUserIdByUsername(username);

	    // Xác định trạng thái được chọn từ key
	    QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);

	    // Tạo Pageable để phân trang và sắp xếp
	    Sort sort = Sort.by(Sort.Direction.fromString(sortDir), sortBy);
	    Pageable pageable = PageRequest.of(page, size, sort);

	    // Lọc câu hỏi dựa trên trạng thái được chọn
	    Page<MyQuestionDTO> questions;

	    switch (filterStatus) {
	        case ANSWERED:
	            questions = questionService.findAnsweredQuestions(userId, pageable);
	            break;
	        case NOT_ANSWERED:
	            questions = questionService.findNotAnsweredQuestions(userId, pageable);
	            break;
	        case PRIVATE:
	            questions = questionService.findByUserIdAndStatusPublic(userId, false, pageable);
	            break;
	        case PUBLIC:
	            questions = questionService.findByUserIdAndStatusPublic(userId, true, pageable);
	            break;
	        case DELETED:
	            questions = questionService.findByUserIdAndStatusDelete(userId, true, pageable);
	            break;
	        case NOT_APPROVED:
	            questions = questionService.findByUserIdAndStatusApproval(userId, false, pageable);
	            break;
	        case APPROVED:
	            questions = questionService.findByUserIdAndStatusApproval(userId, true, pageable);
	            break;
	        default:
	            throw new IllegalArgumentException("Unknown filter status: " + status);
	    }

	    if (questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error").message("No questions found for the given status: " + status).build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder()
	            .status("success").message("Filtered questions by status successfully.").data(questions).build();

	    return ResponseEntity.ok(response);
	}




	@GetMapping("/filter-status-options")
	public ResponseEntity<DataResponse<List<QuestionStatusDTO>>> getFilterStatusOptions() {
	    // Lấy danh sách các trạng thái từ enum
	    List<QuestionStatusDTO> statuses = Arrays.stream(QuestionFilterStatus.values())
	            .map(status -> new QuestionStatusDTO(status.getKey(), status.getDisplayName()))
	            .collect(Collectors.toList());

	    DataResponse<List<QuestionStatusDTO>> response = DataResponse.<List<QuestionStatusDTO>>builder()
	            .status("success")
	            .message("Fetched all filter status options successfully.")
	            .data(statuses)
	            .build();

	    return ResponseEntity.ok(response);
	}

}
