package studentConsulting.service.implement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.DeletionLogEntity;
import studentConsulting.model.entity.questionAnswer.ForwardQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.DeletionLogRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.FieldRepository;
import studentConsulting.repository.ForwardQuestionRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RoleAskRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IQuestionService;
import studentConsulting.specification.ConsultantSpecification;

@Service
public class QuestionServiceImpl implements IQuestionService {

	private static final String UPLOAD_DIR = "D:/HCMUTE-K21/DoAnGitHub/Nam4HocKy1TLCN/upload/"; // Đường dẫn lưu file

	@Autowired
	private QuestionRepository questionRepository;

	@Autowired
	private UserRepository userRepository;

	@Autowired
	private DepartmentRepository departmentRepository;

	@Autowired
	private FieldRepository fieldRepository;

	@Autowired
	private RoleAskRepository roleAskRepository;

	@Autowired
	private AnswerRepository answerRepository;
	
	@Autowired
    private DeletionLogRepository deletionLogRepository;
	
	@Autowired
    private ForwardQuestionRepository forwardQuestionRepository;

	@Override
    public Page<MyQuestionDTO> getAllQuestions(Pageable pageable) {
        Page<QuestionEntity> questions = questionRepository.findAll(pageable);
        return questions.map(this::mapToMyQuestionDTO);
    }
	
	@Override
	public Page<MyQuestionDTO> filterAllQuestionsByDepartment(Integer departmentId, Pageable pageable) {
	    Page<QuestionEntity> questions = questionRepository.findByDepartmentId(departmentId, pageable);
	    return questions.map(this::mapToMyQuestionDTO);
	}
	
	@Override
	public DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest) {
		// Lưu file nếu có
		String fileName = null;
		if (questionRequest.getFile() != null && !questionRequest.getFile().isEmpty()) {
			fileName = saveFile(questionRequest.getFile());
		}

		QuestionDTO questionDTO = mapRequestToDTO(questionRequest, fileName);
		QuestionEntity question = mapDTOToEntity(questionDTO);
		question.setStatusApproval(false);
		question.setViews(0); // Thiết lập views là 0 khi tạo câu hỏi mới

		QuestionEntity savedQuestion = questionRepository.save(question);
		savedQuestion.setParentQuestion(savedQuestion);
		questionRepository.save(savedQuestion);
		QuestionDTO savedQuestionDTO = mapEntityToDTO(savedQuestion);

		return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi đã được tạo")
				.data(savedQuestionDTO).build();
	}

	private String saveFile(MultipartFile file) {
		try {
			String fileName = file.getOriginalFilename();
			Path path = Paths.get(UPLOAD_DIR + fileName);

			// Kiểm tra và tạo thư mục nếu chưa tồn tại
			if (Files.notExists(path.getParent())) {
				Files.createDirectories(path.getParent());
			}

			// Lưu file vào đường dẫn đã định nghĩa
			Files.write(path, file.getBytes());

			return fileName;
		} catch (IOException e) {
			throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
		}
	}

	private QuestionDTO mapRequestToDTO(CreateQuestionRequest request, String fileName) {
		return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
				.roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
				.firstName(request.getFirstName()).lastName(request.getLastName()).studentCode(request.getStudentCode())
				.statusPublic(request.getStatusPublic()).fileName(fileName).statusApproval(false).build();
	}

	private QuestionEntity mapDTOToEntity(QuestionDTO questionDTO) {
		QuestionEntity question = new QuestionEntity();
		question.setTitle(questionDTO.getTitle());
		question.setContent(questionDTO.getContent());
		question.setStatusPublic(questionDTO.getStatusPublic());
		question.setViews(questionDTO.getViews());

		UserInformationEntity user = findStudentCode(questionDTO.getStudentCode());
		user.setFirstName(questionDTO.getFirstName());
		user.setLastName(questionDTO.getLastName());
		question.setUser(user);

		question.setDepartment(findDepartmentById(questionDTO.getDepartmentId()));
		question.setField(findFieldById(questionDTO.getFieldId()));
		question.setRoleAsk(findRoleAskById(questionDTO.getRoleAskId()));
		question.setFileName(questionDTO.getFileName());
		question.setCreatedAt(LocalDateTime.now());
		question.setUpdatedAt(LocalDateTime.now());

		return question;
	}

	private QuestionDTO mapEntityToDTO(QuestionEntity question) {
		return QuestionDTO.builder().departmentId(question.getDepartment().getId()).fieldId(question.getField().getId())
				.roleAskId(question.getRoleAsk().getId()).title(question.getTitle()).content(question.getContent())
				.firstName(question.getUser().getFirstName()).lastName(question.getUser().getLastName())
				.studentCode(question.getUser().getStudentCode()).statusPublic(question.getStatusPublic())
				.fileName(question.getFileName()).statusApproval(question.getStatusApproval()).build();
	}

	@Override
	public UserInformationEntity findStudentCode(String studentCode) {
		return userRepository.findByStudentCode(studentCode)
				.orElseThrow(() -> new RuntimeException("Student code not found with code: " + studentCode));
	}

	@Override
	public DepartmentEntity findDepartmentById(Integer id) {
		return departmentRepository.findById(id)
				.orElseThrow(() -> new RuntimeException("Department not found with id: " + id));
	}

	@Override
	public FieldEntity findFieldById(Integer id) {
		return fieldRepository.findById(id).orElseThrow(() -> new RuntimeException("Field not found with id: " + id));
	}

	@Override
	public RoleAskEntity findRoleAskById(Integer id) {
		return roleAskRepository.findById(id)
				.orElseThrow(() -> new RuntimeException("RoleAsk not found with id: " + id));
	}

	@Override
	public DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request) {
		QuestionEntity existingQuestion = questionRepository.findById(questionId)
				.orElseThrow(() -> new RuntimeException("Câu hỏi không tồn tại"));

		System.out.println("Trước khi cập nhật:");
		System.out.println("Title: " + existingQuestion.getTitle());
		System.out.println("Content: " + existingQuestion.getContent());
		System.out.println("Status Public: " + existingQuestion.getStatusPublic());
		System.out.println("Department: " + existingQuestion.getDepartment().getId());
		System.out.println("Field: " + existingQuestion.getField().getId());
		System.out.println("RoleAsk: " + existingQuestion.getRoleAsk().getId());
		System.out.println("FileName: " + existingQuestion.getFileName());
		System.out.println("StudentCode: " + existingQuestion.getUser().getStudentCode());
		System.out.println("FirstName: " + existingQuestion.getUser().getFirstName());
		System.out.println("LastName: " + existingQuestion.getUser().getLastName());

		if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
			return DataResponse.<QuestionDTO>builder().status("error")
					.message("Câu hỏi đã được duyệt, không thể chỉnh sửa.").build();
		}

		existingQuestion.setTitle(request.getTitle());
		existingQuestion.setContent(request.getContent());
		existingQuestion.setStatusPublic(request.getStatusPublic());

		existingQuestion.setDepartment(findDepartmentById(request.getDepartmentId()));
		existingQuestion.setField(findFieldById(request.getFieldId()));
		existingQuestion.setRoleAsk(findRoleAskById(request.getRoleAskId()));

		UserInformationEntity user = existingQuestion.getUser();
		user.setStudentCode(request.getStudentCode());
		user.setFirstName(request.getFirstName());
		user.setLastName(request.getLastName());
		existingQuestion.setUser(user);

		if (request.getFile() != null && !request.getFile().isEmpty()) {
			String fileName = saveFile(request.getFile());
			existingQuestion.setFileName(fileName);
		}
		existingQuestion.setViews(existingQuestion.getViews());
		existingQuestion.setStatusApproval(false);

		QuestionEntity updatedQuestion = questionRepository.save(existingQuestion);
		QuestionDTO updatedQuestionDTO = mapEntityToDTO(updatedQuestion);

		System.out.println("Sau khi cập nhật:");
		System.out.println("Title: " + updatedQuestion.getTitle());
		System.out.println("Content: " + updatedQuestion.getContent());
		System.out.println("Status Public: " + updatedQuestion.getStatusPublic());
		System.out.println("Department: " + updatedQuestion.getDepartment().getId());
		System.out.println("Field: " + updatedQuestion.getField().getId());
		System.out.println("RoleAsk: " + updatedQuestion.getRoleAsk().getId());
		System.out.println("FileName: " + updatedQuestion.getFileName());
		System.out.println("StudentCode: " + updatedQuestion.getUser().getStudentCode());
		System.out.println("FirstName: " + updatedQuestion.getUser().getFirstName());
		System.out.println("LastName: " + updatedQuestion.getUser().getLastName());

		return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi đã được cập nhật thành công.")
				.data(updatedQuestionDTO).build();
	}

	@Override
	@Transactional
	public DataResponse<Void> deleteQuestion(Integer questionId, String username) {
	    // Tìm câu hỏi dựa trên questionId
	    QuestionEntity existingQuestion = questionRepository.findById(questionId)
	            .orElseThrow(() -> new RuntimeException("Câu hỏi không tồn tại"));

	    // Kiểm tra xem câu hỏi đã bị xóa trước đó chưa
	    Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
	    if (existingLog.isPresent()) {
	        return DataResponse.<Void>builder()
	                .status("error")
	                .message("Câu hỏi đã bị xóa trước đó.")
	                .build();
	    }

	    // Kiểm tra trạng thái phê duyệt của câu hỏi
	    if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
	        return DataResponse.<Void>builder()
	                .status("error")
	                .message("Câu hỏi đã được duyệt, không thể xóa.")
	                .build();
	    }

	    // Tìm thông tin người dùng thực hiện thao tác xóa
	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    if (userOpt.isEmpty()) {
	        return DataResponse.<Void>builder()
	                .status("error")
	                .message("User not found.")
	                .build();
	    }
	    UserInformationEntity user = userOpt.get();

	    // Lưu thông tin vào DeletionLog
	    DeletionLogEntity deletionLog = DeletionLogEntity.builder()
	            .question(existingQuestion) // Lưu reference đến câu hỏi
	            .reason("Xóa theo yêu cầu của bản thân") // Lý do xóa
	            .deletedBy(user.getAccount().getUsername()) // Người thực hiện xóa
	            .deletedAt(LocalDateTime.now()) // Thời gian xóa
	            .build();

	    deletionLogRepository.save(deletionLog);

	    // Sau khi đã lưu vào deletion_log, thực hiện xóa câu hỏi (xóa logic)
	    questionRepository.softDeleteQuestion(questionId);

	    return DataResponse.<Void>builder()
	            .status("success")
	            .message("Question deleted successfully.")
	            .build();
	}


	@Override
	public List<RoleAskDTO> getAllRoleAsk() {
		return roleAskRepository.findAll().stream().map(roleAsk -> new RoleAskDTO(roleAsk.getId(), roleAsk.getName()))
				.collect(Collectors.toList());
	}

	@Override
	public DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content,
			MultipartFile file) {
		// Tìm câu hỏi cha
		QuestionEntity parentQuestion = questionRepository.findById(parentQuestionId)
				.orElseThrow(() -> new RuntimeException("Câu hỏi cha không tồn tại"));

		// Tạo request cho câu hỏi follow-up dựa trên thông tin từ câu hỏi cha
		CreateFollowUpQuestionRequest followUpRequest = CreateFollowUpQuestionRequest.builder()
				.parentQuestionId(parentQuestionId).departmentId(parentQuestion.getDepartment().getId())
				.fieldId(parentQuestion.getField().getId()).roleAskId(parentQuestion.getRoleAsk().getId())
				.firstName(parentQuestion.getUser().getFirstName()).lastName(parentQuestion.getUser().getLastName())
				.studentCode(parentQuestion.getUser().getStudentCode()).title(title).content(content)
				.statusPublic(parentQuestion.getStatusPublic()).file(file).statusApproval(false).build();

		// Lưu câu hỏi follow-up vào cơ sở dữ liệu
		QuestionDTO followUpQuestionDTO = mapRequestToDTO(followUpRequest,
				file != null ? file.getOriginalFilename() : null);
		QuestionEntity followUpQuestion = mapDTOToEntity(followUpQuestionDTO);

		followUpQuestion.setStatusApproval(false);
		followUpQuestion.setViews(parentQuestion.getViews()); // Lấy số lượt xem từ câu hỏi cha

		// Liên kết với câu hỏi cha
		followUpQuestion.setParentQuestion(parentQuestion);

		// Lưu câu hỏi follow-up
		QuestionEntity savedFollowUpQuestion = questionRepository.save(followUpQuestion);

		// Chuyển entity sang DTO để trả về
		QuestionDTO savedFollowUpQuestionDTO = mapEntityToDTO(savedFollowUpQuestion);

		return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi tiếp theo đã được tạo")
				.data(savedFollowUpQuestionDTO).build();
	}

	private QuestionDTO mapRequestToDTO(CreateFollowUpQuestionRequest request, String fileName) {
		return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
				.roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
				.firstName(request.getFirstName()).lastName(request.getLastName()).studentCode(request.getStudentCode())
				.statusPublic(request.getStatusPublic()).fileName(fileName).build();
	}

	@Override
	public Page<MyQuestionDTO> findAnsweredQuestionsByTitleAndDepartment(Integer userId, String title, Integer departmentId, Pageable pageable) {
	    return questionRepository.findAnsweredQuestionsByTitleAndDepartment(userId, title, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findNotAnsweredQuestionsByTitleAndDepartment(Integer userId, String title, Integer departmentId, Pageable pageable) {
	    return questionRepository.findNotAnsweredQuestionsByTitleAndDepartment(userId, title, departmentId, pageable)
	            .map(this::convertToDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusPublicTitleAndDepartment(Integer userId, boolean isPublic, String title, Integer departmentId, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusPublicTitleAndDepartment(userId, isPublic, title, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusDeleteTitleAndDepartment(Integer userId, boolean isDeleted, String title, Integer departmentId, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusDeleteTitleAndDepartment(userId, isDeleted, title, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusApprovalTitleAndDepartment(Integer userId, boolean isApproved, String title, Integer departmentId, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusApprovalTitleAndDepartment(userId, isApproved, title, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> searchQuestionsByTitle(Integer userId, String title, Pageable pageable) {
	    return questionRepository.searchQuestionsByTitle(userId, title, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findNotAnsweredQuestionsByTitle(Integer userId, String title, Pageable pageable) {
	    return questionRepository.findNotAnsweredQuestionsByTitle(userId, title, pageable)
	            .map(this::convertToDTO);
	}

	@Override
	public Page<MyQuestionDTO> findAnsweredQuestionsByTitle(Integer userId, String title, Pageable pageable) {
	    return questionRepository.findAnsweredQuestionsByTitle(userId, title, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusPublicTitle(Integer userId, boolean isPublic, String title, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusPublicTitle(userId, isPublic, title, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusDeleteTitle(Integer userId, boolean isDeleted, String title, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusDeleteTitle(userId, isDeleted, title, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusApprovalTitle(Integer userId, boolean isApproved, String title, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusApprovalTitle(userId, isApproved, title, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findAnsweredQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable) {
	    return questionRepository.findAnsweredQuestionsByDepartment(userId, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findNotAnsweredQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable) {
	    return questionRepository.findNotAnsweredQuestionsByDepartment(userId, departmentId, pageable)
	            .map(this::convertToDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusPublicAndDepartment(Integer userId, boolean isPublic, Integer departmentId, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusPublicAndDepartment(userId, isPublic, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusDeleteAndDepartment(Integer userId, Boolean isDeleted, Integer departmentId, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusDeleteAndDepartment(userId, isDeleted, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusApprovalAndDepartment(Integer userId, Boolean isApproved, Integer departmentId, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusApprovalAndDepartment(userId, isApproved, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findAnsweredQuestions(Integer userId, Pageable pageable) {
	    return questionRepository.findAnsweredQuestions(userId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findNotAnsweredQuestions(Integer userId, Pageable pageable) {
	    return questionRepository.findNotAnsweredQuestions(userId, pageable)
	            .map(this::convertToDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusPublic(Integer userId, boolean isPublic, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusPublic(userId, isPublic, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusDelete(Integer userId, boolean isDeleted, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusDelete(userId, isDeleted, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> findByUserIdAndStatusApproval(Integer userId, boolean isApproved, Pageable pageable) {
	    return questionRepository.findByUserIdAndStatusApproval(userId, isApproved, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> getQuestionsByUserId(Integer userId, Pageable pageable) {
	    return questionRepository.findByUserId(userId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> searchQuestionsByTitleAndDepartment(Integer userId, String title, Integer departmentId, Pageable pageable) {
	    return questionRepository.searchQuestionsByTitleAndDepartment(userId, title, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

	@Override
	public Page<MyQuestionDTO> filterMyQuestionsByDepartment(Integer userId, Integer departmentId, Pageable pageable) {
	    return questionRepository.filterMyQuestionsByDepartment(userId, departmentId, pageable)
	            .map(this::mapToMyQuestionDTO);
	}

    
	private MyQuestionDTO convertToDTO(QuestionEntity question) {
	    return MyQuestionDTO.builder()
	        .department(MyQuestionDTO.DepartmentDTO.builder()
	            .id(question.getDepartment().getId())
	            .name(question.getDepartment().getName())
	            .build())
	        .field(MyQuestionDTO.FieldDTO.builder()
	            .id(question.getField().getId())
	            .name(question.getField().getName())
	            .build())
	        .roleAsk(MyQuestionDTO.RoleAskDTO.builder()
	            .id(question.getRoleAsk().getId())
	            .name(question.getRoleAsk().getName())
	            .build())
	        .title(question.getTitle())
	        .content(question.getContent())
	        .createdAt(question.getCreatedAt())
	        .views(question.getViews())
	        .fileName(question.getFileName())
	        .askerFirstname(question.getUser().getFirstName())
	        .askerLastname(question.getUser().getLastName())
	        .build();
	}


	public Page<MyQuestionDTO> getQuestionsWithFilters(Integer consultantId, String title, String status, Pageable pageable) {
	    Specification<QuestionEntity> spec = Specification.where(ConsultantSpecification.hasConsultantAnswer(consultantId));

	    if (title != null && !title.isEmpty()) {
	        spec = spec.and(ConsultantSpecification.hasTitle(title));
	    }

	    if (status != null && !status.isEmpty()) {
	        // Chuyển đổi chuỗi thành enum
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);  
	        // Áp dụng tiêu chí lọc theo trạng thái
	        spec = spec.and(ConsultantSpecification.hasStatus(filterStatus));
	    }

	    Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);

	    return questionEntities.map(this::mapToMyQuestionDTO);
	}




	
	private MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question) {
	    // Lấy thông tin của người hỏi từ QuestionEntity
	    String askerFirstname = question.getUser().getFirstName();
	    String askerLastname = question.getUser().getLastName();

	    MyQuestionDTO.DepartmentDTO departmentDTO = MyQuestionDTO.DepartmentDTO.builder()
	        .id(question.getDepartment().getId())
	        .name(question.getDepartment().getName())
	        .build();

	    MyQuestionDTO.FieldDTO fieldDTO = MyQuestionDTO.FieldDTO.builder()
	        .id(question.getField().getId())
	        .name(question.getField().getName())
	        .build();

	    MyQuestionDTO.RoleAskDTO roleAskDTO = MyQuestionDTO.RoleAskDTO.builder()
	        .id(question.getRoleAsk().getId())
	        .name(question.getRoleAsk().getName())
	        .build();

	    MyQuestionDTO dto = MyQuestionDTO.builder()
	        .title(question.getTitle())
	        .content(question.getContent())
	        .createdAt(question.getCreatedAt())
	        .views(question.getViews())
	        .fileName(question.getFileName())
	        .askerFirstname(askerFirstname)
	        .askerLastname(askerLastname)
	        
	        // Ánh xạ các đối tượng department, field và roleAsk
	        .department(departmentDTO)
	        .field(fieldDTO)
	        .roleAsk(roleAskDTO)

	        .build();

	    // Tìm câu trả lời cho câu hỏi này (nếu có)
	    Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(question.getId());
	    answerOpt.ifPresent(answer -> {
	        dto.setAnswerTitle(answer.getTitle());
	        dto.setAnswerContent(answer.getContent());
	        dto.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
	        dto.setAnswerUserFirstname(answer.getUser().getFirstName());
	        dto.setAnswerUserLastname(answer.getUser().getLastName());
	        dto.setAnswerCreatedAt(answer.getCreatedAt());
	    });

	    return dto;
	}

	
	@Override
	@Transactional
	public DataResponse<String> deleteQuestion(Integer questionId, String reason, String username) {

	    // Tìm câu hỏi dựa trên questionId
	    Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
	    if (questionOpt.isEmpty()) {
	        return DataResponse.<String>builder()
	                .status("error")
	                .message("Question not found.")
	                .build();
	    }

	    QuestionEntity question = questionOpt.get();

	    // Kiểm tra xem câu hỏi đã tồn tại trong DeletionLog chưa
	    Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
	    if (existingLog.isPresent()) {
	        return DataResponse.<String>builder()
	                .status("error")
	                .message("Question has already been deleted.")
	                .build();
	    }

	    // Kiểm tra xem câu hỏi đã có câu trả lời chưa
	    Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(questionId);
	    if (answerOpt.isPresent()) {  // Sử dụng isPresent() để kiểm tra có câu trả lời
	        return DataResponse.<String>builder()
	                .status("error")
	                .message("Cannot delete question, it has already been answered.")
	                .build();
	    }

	    
	    
	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    if (userOpt.isEmpty()) {
	        return DataResponse.<String>builder()
	                .status("error")
	                .message("User not found.")
	                .build();
	    }

	    UserInformationEntity user = userOpt.get();

	    if (!"TUVANVIEN".equals(user.getAccount().getRole().getName())) {
	        return DataResponse.<String>builder()
	                .status("error")
	                .message("You do not have permission to delete this question.")
	                .build();
	    }
	    // Lưu thông tin vào DeletionLog
	    DeletionLogEntity deletionLog = DeletionLogEntity.builder()
	            .question(question) // Lưu reference đến câu hỏi
	            .reason(reason) // Lý do xóa
	            .deletedBy(user.getAccount().getUsername()) // Người thực hiện xóa
	            .deletedAt(LocalDateTime.now()) // Thời gian xóa
	            .build();

	    deletionLogRepository.save(deletionLog);

	    // Sau khi đã lưu vào deletion_log, thực hiện xóa câu hỏi (xóa logic)
	    questionRepository.softDeleteQuestion(questionId);

	    return DataResponse.<String>builder()
	            .status("success")
	            .message("Question deleted successfully.")
	            .build();
	}



	@Override
	@Transactional
	public DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username) {
	    // Lấy thông tin người dùng hiện tại
	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    if (userOpt.isEmpty()) {
	        return DataResponse.<ForwardQuestionDTO>builder()
	                .status("error")
	                .message("Người dùng không tồn tại.")
	                .build();
	    }

	    UserInformationEntity user = userOpt.get();

	    // Kiểm tra vai trò của người dùng, chỉ cho phép TUVANVIEN
	    if (!"TUVANVIEN".equals(user.getAccount().getRole().getName())) {
	        return DataResponse.<ForwardQuestionDTO>builder()
	                .status("error")
	                .message("Bạn không có quyền thực hiện chức năng này.")
	                .build();
	    }

	    // Lấy thông tin phòng ban của tư vấn viên hiện tại
	    DepartmentEntity fromDepartment = user.getAccount().getDepartment();
	    if (fromDepartment == null) {
	        throw new ErrorException("Phòng ban của tư vấn viên không tồn tại.");
	    }

	    // Lấy thông tin phòng ban chuyển đến
	    DepartmentEntity toDepartment = departmentRepository.findById(forwardQuestionRequest.getToDepartmentId())
	            .orElseThrow(() -> new ErrorException("Phòng ban chuyển đến không tồn tại"));

	    // Kiểm tra nếu phòng ban chuyển đến là cùng phòng ban với tư vấn viên hiện tại
	    if (fromDepartment.getId().equals(toDepartment.getId())) {
	        throw new ErrorException("Không thể chuyển tiếp câu hỏi đến cùng một phòng ban.");
	    }

	    // Lấy thông tin câu hỏi
	    QuestionEntity question = questionRepository.findById(forwardQuestionRequest.getQuestionId())
	            .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

	    // Lấy thông tin tư vấn viên mới
	    UserInformationEntity consultant = userRepository.findById(forwardQuestionRequest.getConsultantId())
	            .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

	    // Kiểm tra xem tư vấn viên mới có thuộc phòng ban chuyển đến không
	    if (!consultant.getAccount().getDepartment().equals(toDepartment)) {
	        throw new ErrorException("Tư vấn viên không thuộc phòng ban chuyển đến.");
	    }

	    // Tạo mới ForwardQuestionEntity và lưu vào cơ sở dữ liệu
	    ForwardQuestionEntity forwardQuestion = ForwardQuestionEntity.builder()
	            .fromDepartment(fromDepartment)
	            .toDepartment(toDepartment)
	            .question(question)
	            .title("Đã chuyển tiếp câu hỏi từ " + fromDepartment.getName() + " cho " + toDepartment.getName()) // Tạo tiêu đề tự động
	            .statusForward(true)
	            .createdAt(LocalDateTime.now())
	            .build();

	    forwardQuestionRepository.save(forwardQuestion);

	    // Tạo ForwardQuestionDTO và trả về
	    ForwardQuestionDTO forwardQuestionDTO = mapToForwardQuestionDTO(forwardQuestion);

	    return DataResponse.<ForwardQuestionDTO>builder()
	            .status("success")
	            .message("Câu hỏi đã được chuyển tiếp thành công.")
	            .data(forwardQuestionDTO)
	            .build();
	}



	

	private ForwardQuestionDTO mapToForwardQuestionDTO(ForwardQuestionEntity forwardQuestion) {
	    ForwardQuestionDTO.DepartmentDTO fromDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
	        .id(forwardQuestion.getFromDepartment().getId())
	        .name(forwardQuestion.getFromDepartment().getName())
	        .build();

	    ForwardQuestionDTO.DepartmentDTO toDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
	        .id(forwardQuestion.getToDepartment().getId())
	        .name(forwardQuestion.getToDepartment().getName())
	        .build();

	    ForwardQuestionDTO.ConsultantDTO consultantDTO = ForwardQuestionDTO.ConsultantDTO.builder()
	        .id(forwardQuestion.getQuestion().getUser().getId())
	        .firstName(forwardQuestion.getQuestion().getUser().getFirstName())
	        .lastName(forwardQuestion.getQuestion().getUser().getLastName())
	        .build();

	    // Tạo ForwardQuestionDTO
	    return ForwardQuestionDTO.builder()
	        .title(forwardQuestion.getTitle())
	        .fromDepartment(fromDepartmentDTO)
	        .toDepartment(toDepartmentDTO)
	        .consultant(consultantDTO)
	        .statusForward(forwardQuestion.getStatusForward())
	        .build();
	}
	
	



}