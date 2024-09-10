package studentConsulting.controller;

import java.security.Principal;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.DeletionLogDTO;
import studentConsulting.model.payload.dto.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.QuestionStatusDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IQuestionService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("/api/v1/question")
public class QuestionController {

	@Autowired
	private IQuestionService questionService;

	@Autowired
	private IUserService userService;

	@Autowired
	private UserRepository userRepository;
	
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
	public ResponseEntity<DataResponse<Void>> deleteQuestion(@PathVariable("id") Integer questionId, Principal principal) {
	    // Lấy tên người dùng từ Principal
	    String username = principal.getName();
	    
	    // Gọi service để thực hiện thao tác xóa câu hỏi, truyền questionId và username
	    return ResponseEntity.ok(questionService.deleteQuestion(questionId, username));
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

	@GetMapping("/list/user")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> getQuestions(
	        Principal principal,
	        @RequestParam(required = false) String title,
	        @RequestParam(required = false) Integer departmentId,
	        @RequestParam(required = false) String status,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();
	    Integer userId = userService.getUserIdByUsername(username);

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Biến lưu câu hỏi sau khi lọc
	    Page<MyQuestionDTO> questions = null;

	    // Trường hợp lọc theo `status`, `title`, và `departmentId`
	    if (status != null && title != null && departmentId != null) {
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
	        switch (filterStatus) {
	            case ANSWERED:
	                questions = questionService.findAnsweredQuestionsByTitleAndDepartment(userId, title, departmentId, pageable);
	                break;
	            case NOT_ANSWERED:
	                questions = questionService.findNotAnsweredQuestionsByTitleAndDepartment(userId, title, departmentId, pageable);
	                break;
	            case PRIVATE:
	                questions = questionService.findByUserIdAndStatusPublicTitleAndDepartment(userId, false, title, departmentId, pageable);
	                break;
	            case PUBLIC:
	                questions = questionService.findByUserIdAndStatusPublicTitleAndDepartment(userId, true, title, departmentId, pageable);
	                break;
	            case DELETED:
	                questions = questionService.findByUserIdAndStatusDeleteTitleAndDepartment(userId, true, title, departmentId, pageable);
	                break;
	            case APPROVED:
	                questions = questionService.findByUserIdAndStatusApprovalTitleAndDepartment(userId, true, title, departmentId, pageable);
	                break;
	            default:
	                throw new IllegalArgumentException("Unknown filter status: " + status);
	        }
	    } 
	    // Trường hợp chỉ có `status` và `title`
	    else if (status != null && title != null) {
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
	        switch (filterStatus) {
	            case ANSWERED:
	                questions = questionService.findAnsweredQuestionsByTitle(userId, title, pageable);
	                break;
	            case NOT_ANSWERED:
	                questions = questionService.findNotAnsweredQuestionsByTitle(userId, title, pageable);
	                break;
	            case PRIVATE:
	                questions = questionService.findByUserIdAndStatusPublicTitle(userId, false, title, pageable);
	                break;
	            case PUBLIC:
	                questions = questionService.findByUserIdAndStatusPublicTitle(userId, true, title, pageable);
	                break;
	            case DELETED:
	                questions = questionService.findByUserIdAndStatusDeleteTitle(userId, true, title, pageable);
	                break;
	            case APPROVED:
	                questions = questionService.findByUserIdAndStatusApprovalTitle(userId, true, title, pageable);
	                break;
	            default:
	                throw new IllegalArgumentException("Unknown filter status: " + status);
	        }
	    } 
	    // Trường hợp chỉ có `status` và `departmentId`
	    else if (status != null && departmentId != null) {
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
	        switch (filterStatus) {
	            case ANSWERED:
	                questions = questionService.findAnsweredQuestionsByDepartment(userId, departmentId, pageable);
	                break;
	            case NOT_ANSWERED:
	                questions = questionService.findNotAnsweredQuestionsByDepartment(userId, departmentId, pageable);
	                break;
	            case PRIVATE:
	                questions = questionService.findByUserIdAndStatusPublicAndDepartment(userId, false, departmentId, pageable);
	                break;
	            case PUBLIC:
	                questions = questionService.findByUserIdAndStatusPublicAndDepartment(userId, true, departmentId, pageable);
	                break;
	            case DELETED:
	                questions = questionService.findByUserIdAndStatusDeleteAndDepartment(userId, true, departmentId, pageable);
	                break;
	            case APPROVED:
	                questions = questionService.findByUserIdAndStatusApprovalAndDepartment(userId, true, departmentId, pageable);
	                break;
	            default:
	                throw new IllegalArgumentException("Unknown filter status: " + status);
	        }
	    } 
	    // Trường hợp chỉ có `title` và `departmentId`
	    else if (title != null && departmentId != null) {
	        questions = questionService.searchQuestionsByTitleAndDepartment(userId, title, departmentId, pageable);
	    } 
	    // Trường hợp chỉ có `status`
	    else if (status != null) {
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
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
	            case APPROVED:
	                questions = questionService.findByUserIdAndStatusApproval(userId, true, pageable);
	                break;
	            default:
	                throw new IllegalArgumentException("Unknown filter status: " + status);
	        }
	    } 
	    // Trường hợp chỉ có `title`
	    else if (title != null) {
	        questions = questionService.searchQuestionsByTitle(userId, title, pageable);
	    } 
	    // Trường hợp chỉ có `departmentId`
	    else if (departmentId != null) {
	        questions = questionService.filterMyQuestionsByDepartment(userId, departmentId, pageable);
	    } 
	    // Nếu không có điều kiện lọc nào, lấy tất cả câu hỏi của người dùng
	    else {
	        questions = questionService.getQuestionsByUserId(userId, pageable);
	    }

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (questions == null || questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error")
	                .message("No questions found.")
	                .build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi của người dùng có phân trang
	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder()
	            .status("success")
	            .message("Fetched questions successfully.")
	            .data(questions)
	            .build();

	    return ResponseEntity.ok(response);
	}


	@GetMapping("/list/consultant")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> getQuestions(
	        Principal principal,
	        @RequestParam(required = false) String title,
	        @RequestParam(required = false) String status,  // Chuỗi status từ client
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();
	    
	    // Lấy thông tin người dùng và kiểm tra vai trò
	    Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
	    
	    if (userOptional.isEmpty()) {
	        return ResponseEntity.status(HttpStatus.NOT_FOUND)
	                             .body(DataResponse.<Page<MyQuestionDTO>>builder()
	                             .status("error")
	                             .message("User not found.")
	                             .build());
	    }
	    
	    UserInformationEntity user = userOptional.get();
	    String roleName = user.getAccount().getRole().getName();
	    
	    // Kiểm tra nếu người dùng không phải là TUVANVIEN
	    if (!roleName.equals("TUVANVIEN")) {
	        return ResponseEntity.status(HttpStatus.FORBIDDEN)
	                             .body(DataResponse.<Page<MyQuestionDTO>>builder()
	                             .status("error")
	                             .message("You do not have permission to access this resource.")
	                             .build());
	    }

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Chuyển chuỗi status thành enum
	    QuestionFilterStatus filterStatus = null;
	    if (status != null && !status.isEmpty()) {
	        filterStatus = QuestionFilterStatus.fromKey(status);
	    }

	    // Lấy danh sách câu hỏi với các tiêu chí lọc
	    Page<MyQuestionDTO> questions = questionService.getQuestionsWithFilters(user.getId(), title, filterStatus != null ? filterStatus.getKey() : null, pageable);

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (questions == null || questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error")
	                .message("No questions found.")
	                .build();
	        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi của người dùng có phân trang
	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder()
	            .status("success")
	            .message("Fetched questions successfully.")
	            .data(questions)
	            .build();

	    return ResponseEntity.ok(response);
	}









	
	@GetMapping("/list")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> getQuestions(
	        @RequestParam(required = false) Integer departmentId,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Kiểm tra nếu có departmentId, lọc theo phòng ban
	    Page<MyQuestionDTO> questions;
	    if (departmentId != null) {
	        questions = questionService.filterAllQuestionsByDepartment(departmentId, pageable);
	    } else {
	        // Nếu không có departmentId, lấy tất cả câu hỏi
	        questions = questionService.getAllQuestions(pageable);
	    }

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error").message("No questions found.").build();
	        return ResponseEntity.status(404).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi có phân trang
	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder()
	            .status("success")
	            .message(departmentId != null ? "Filtered questions by department successfully." : "Fetched all common questions successfully.")
	            .data(questions)
	            .build();

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
	
	@DeleteMapping("/delete")
	public ResponseEntity<DataResponse<String>> deleteQuestion(
	        @RequestParam("questionId") Integer questionId,
	        @RequestParam("reason") String reason,
	        Principal principal) {

	    // Kiểm tra xem lý do xóa có được cung cấp hay không
	    if (reason == null || reason.trim().isEmpty()) {
	        return ResponseEntity.ok(
	            DataResponse.<String>builder()
	                .status("error")
	                .message("Reason for deletion is required.")
	                .build()
	        );
	    }

	    // Lấy username của người dùng hiện tại
	    String username = principal.getName();

	    // Gọi service để xử lý việc xóa câu hỏi
	    DataResponse<String> response = questionService.deleteQuestion(questionId, reason, username);

	    // Trả về phản hồi với mã trạng thái 200 OK trong mọi trường hợp
	    return ResponseEntity.ok(response);
	}

	
	@PostMapping("/forward")
	public ResponseEntity<DataResponse<ForwardQuestionDTO>> forwardQuestion(
	        @RequestBody ForwardQuestionRequest forwardQuestionRequest, Principal principal) {
	    
	    // Lấy tên người dùng từ Principal
	    String username = principal.getName();
	    
	    // Gọi service để thực hiện việc chuyển tiếp, kiểu trả về phải là ForwardQuestionDTO
	    DataResponse<ForwardQuestionDTO> response = questionService.forwardQuestion(forwardQuestionRequest, username);
	    
	    // Trả về kết quả
	    return ResponseEntity.ok(response);
	}

	// Hàm kiểm tra nếu người dùng là TƯ VẤN VIÊN
	private boolean isConsultant(UserInformationEntity user) {
	    return "TUVANVIEN".equals(user.getAccount().getRole().getName());
	}

	
	@GetMapping("/list-deleted")
	public ResponseEntity<DataResponse<Page<DeletionLogDTO>>> getDeletedQuestions(
	        Principal principal,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "deletedAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();

	    // Lấy thông tin người dùng
	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    if (userOpt.isEmpty()) {
	        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
	            DataResponse.<Page<DeletionLogDTO>>builder()
	            .status("error")
	            .message("User not found.")
	            .build()
	        );
	    }

	    UserInformationEntity user = userOpt.get();

	    // Kết hợp lastname và firstname để tạo fullname
	    String fullName = user.getLastName() + " " + user.getFirstName();

	    // In ra console để kiểm tra giá trị của fullName
	    System.out.println("Full Name (LastName + FirstName): " + fullName);

	    if (!isConsultant(user)) {
	        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
	            DataResponse.<Page<DeletionLogDTO>>builder()
	            .status("error")
	            .message("You do not have permission to access this resource.")
	            .build()
	        );
	    }
	    
	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Lấy danh sách câu hỏi đã xóa bởi tư vấn viên có fullname tương ứng
	    Page<DeletionLogDTO> deletedQuestions = questionService.getDeletedQuestionsByConsultantFullName(fullName, pageable);

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (deletedQuestions == null || deletedQuestions.isEmpty()) {
	        DataResponse<Page<DeletionLogDTO>> errorResponse = DataResponse.<Page<DeletionLogDTO>>builder()
	                .status("error")
	                .message("No deleted questions found.")
	                .build();
	        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi đã xóa có phân trang
	    DataResponse<Page<DeletionLogDTO>> response = DataResponse.<Page<DeletionLogDTO>>builder()
	            .status("success")
	            .message("Fetched deleted questions successfully.")
	            .data(deletedQuestions)
	            .build();

	    return ResponseEntity.ok(response);
	}





	@GetMapping("/list-forwarded")
	public ResponseEntity<DataResponse<Page<ForwardQuestionDTO>>> getForwardedQuestions(
	        Principal principal,
	        @RequestParam(required = false) String title,
	        @RequestParam(required = false) Integer toDepartmentId,
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();

	    // Lấy thông tin người dùng
	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    if (userOpt.isEmpty()) {
	        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
	            DataResponse.<Page<ForwardQuestionDTO>>builder()
	            .status("error")
	            .message("User not found.")
	            .build()
	        );
	    }

	    UserInformationEntity user = userOpt.get();

	    // Kiểm tra xem người dùng có phải là TƯ VẤN VIÊN không
	    if (!isConsultant(user)) {
	        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
	            DataResponse.<Page<ForwardQuestionDTO>>builder()
	            .status("error")
	            .message("You do not have permission to access this resource.")
	            .build()
	        );
	    }

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Lấy danh sách câu hỏi đã chuyển tiếp theo phòng ban (department) chuyển đến
	    Page<ForwardQuestionDTO> forwardedQuestions = questionService.getForwardedQuestionsByDepartment(title, toDepartmentId, pageable);

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (forwardedQuestions == null || forwardedQuestions.isEmpty()) {
	        DataResponse<Page<ForwardQuestionDTO>> errorResponse = DataResponse.<Page<ForwardQuestionDTO>>builder()
	                .status("error")
	                .message("No forwarded questions found.")
	                .build();
	        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi đã chuyển tiếp có phân trang
	    DataResponse<Page<ForwardQuestionDTO>> response = DataResponse.<Page<ForwardQuestionDTO>>builder()
	            .status("success")
	            .message("Fetched forwarded questions successfully.")
	            .data(forwardedQuestions)
	            .build();

	    return ResponseEntity.ok(response);
	}




	
	@GetMapping("/list/department-questions")
	public ResponseEntity<DataResponse<Page<MyQuestionDTO>>> getDepartmentConsultantsQuestions(
	        Principal principal,
	        @RequestParam(required = false) String title,
	        @RequestParam(required = false) String status,  // Chuỗi status từ client
	        @RequestParam(defaultValue = "0") int page,
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "createdAt") String sortBy,
	        @RequestParam(defaultValue = "desc") String sortDir) {

	    // Lấy username từ Principal
	    String username = principal.getName();

	    // Lấy thông tin người dùng từ cơ sở dữ liệu
	    Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);

	    if (userOptional.isEmpty()) {
	        return ResponseEntity.status(HttpStatus.NOT_FOUND)
	                             .body(DataResponse.<Page<MyQuestionDTO>>builder()
	                             .status("error")
	                             .message("User not found.")
	                             .build());
	    }

	    UserInformationEntity user = userOptional.get();
	    String roleName = user.getAccount().getRole().getName();
	    
	    // Kiểm tra nếu người dùng không phải là TRUONGBANTUVAN
	    if (!roleName.equals("TRUONGBANTUVAN")) {
	        return ResponseEntity.status(HttpStatus.FORBIDDEN)
	                             .body(DataResponse.<Page<MyQuestionDTO>>builder()
	                             .status("error")
	                             .message("You do not have permission to access this resource.")
	                             .build());
	    }

	    // Lấy department của Trưởng Ban Tư Vấn
	    Integer departmentId = user.getAccount().getDepartment().getId();

	    // Tạo Pageable để phân trang và sắp xếp
	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

	    // Chuyển chuỗi status thành enum
	    QuestionFilterStatus filterStatus = null;
	    if (status != null && !status.isEmpty()) {
	        filterStatus = QuestionFilterStatus.fromKey(status);
	    }

	    // Lấy danh sách câu hỏi của tất cả tư vấn viên trong cùng department
	    Page<MyQuestionDTO> questions = questionService.getQuestionsByDepartment(departmentId, title, filterStatus != null ? filterStatus.getKey() : null, pageable);

	    // Kiểm tra nếu danh sách câu hỏi rỗng
	    if (questions == null || questions.isEmpty()) {
	        DataResponse<Page<MyQuestionDTO>> errorResponse = DataResponse.<Page<MyQuestionDTO>>builder()
	                .status("error")
	                .message("No questions found.")
	                .build();
	        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(errorResponse);
	    }

	    // Trả về danh sách câu hỏi có phân trang
	    DataResponse<Page<MyQuestionDTO>> response = DataResponse.<Page<MyQuestionDTO>>builder()
	            .status("success")
	            .message("Fetched questions successfully.")
	            .data(questions)
	            .build();

	    return ResponseEntity.ok(response);
	}


}
