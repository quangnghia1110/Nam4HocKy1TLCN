package studentConsulting.service.implement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.FieldRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RoleAskRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IQuestionService;

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
	public DataResponse<Void> deleteQuestion(Integer questionId) {
		QuestionEntity existingQuestion = questionRepository.findById(questionId)
				.orElseThrow(() -> new RuntimeException("Câu hỏi không tồn tại"));

		if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
			return DataResponse.<Void>builder().status("error").message("Câu hỏi đã được duyệt, không thể xóa.")
					.build();
		}

		questionRepository.delete(existingQuestion);
		return DataResponse.<Void>builder().status("success").message("Câu hỏi đã được xóa thành công.").build();
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



}