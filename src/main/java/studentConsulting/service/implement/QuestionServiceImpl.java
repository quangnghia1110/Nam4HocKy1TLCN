package studentConsulting.service.implement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cloudinary.Cloudinary;

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
import studentConsulting.model.payload.dto.DeletionLogDTO;
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
import studentConsulting.specification.DeletionLogSpecification;
import studentConsulting.specification.ForwardQuestionSpecification;

@Service
public class QuestionServiceImpl implements IQuestionService {

	@Autowired
	private Cloudinary cloudinary;
	
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
	public List<RoleAskDTO> getAllRoleAsk() {
		return roleAskRepository.findAll().stream().map(roleAsk -> new RoleAskDTO(roleAsk.getId(), roleAsk.getName()))
				.collect(Collectors.toList());
	}
	
	@Override
	public DepartmentEntity findDepartmentById(Integer id) {
	    return departmentRepository.findById(id)
	            .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại với id: " + id));
	}

	@Override
	public FieldEntity findFieldById(Integer id) {
	    return fieldRepository.findById(id)
	            .orElseThrow(() -> new ErrorException("Lĩnh vực không tồn tại với id: " + id));
	}

	@Override
	public RoleAskEntity findRoleAskById(Integer id) {
	    return roleAskRepository.findById(id)
	            .orElseThrow(() -> new ErrorException("Vai trò không tồn tại với id: " + id));
	}

	
	
	
	@Override
	public DataResponse<QuestionDTO> createQuestion(CreateQuestionRequest questionRequest, Integer userId) {
	    String fileName = null;
	    if (questionRequest.getFile() != null && !questionRequest.getFile().isEmpty()) {
	        fileName = saveFile(questionRequest.getFile());
	    }

	    QuestionDTO questionDTO = mapRequestToDTO(questionRequest, fileName);
	    QuestionEntity question = mapDTOToEntity(questionDTO, userId);
	    question.setStatusApproval(false);
	    question.setViews(0);

	    QuestionEntity savedQuestion = questionRepository.save(question);
	    savedQuestion.setParentQuestion(savedQuestion);
	    questionRepository.save(savedQuestion);

	    QuestionDTO savedQuestionDTO = mapEntityToDTO(savedQuestion);

	    return DataResponse.<QuestionDTO>builder()
	            .status("success")
	            .message("Câu hỏi đã được tạo")
	            .data(savedQuestionDTO)
	            .build();
	}

	private String saveFile(MultipartFile file) {
	    try {
	        String fileType = file.getContentType();

	        if (fileType != null && fileType.startsWith("image")) {
	            Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
	            String fileUrl = (String) uploadResult.get("url");
	            return fileUrl; 
	        } else {
	            String uploadDir = "D:\\HCMUTE-K21\\DoAnGitHub\\Nam4HocKy1TLCN\\upload";
	            Path uploadPath = Paths.get(uploadDir);

	            if (!Files.exists(uploadPath)) {
	                Files.createDirectories(uploadPath);
	            }

	            String originalFileName = file.getOriginalFilename();
	            Path filePath = uploadPath.resolve(originalFileName);

	            Files.write(filePath, file.getBytes());

	            return filePath.toString();  
	        }
	    } catch (IOException e) {
	        throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
	    }
	}


	@Override
	public DataResponse<QuestionDTO> updateQuestion(Integer questionId, UpdateQuestionRequest request) {
		QuestionEntity existingQuestion = questionRepository.findById(questionId)
		        .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

		if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
		    throw new ErrorException("Câu hỏi đã được duyệt, không thể chỉnh sửa.");
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

		return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi đã được cập nhật thành công.")
				.data(updatedQuestionDTO).build();
	}

	@Override
	@Transactional
	public DataResponse<Void> deleteQuestion(Integer questionId, String username) {
	    QuestionEntity existingQuestion = questionRepository.findById(questionId)
	            .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

	    Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
	    if (existingLog.isPresent()) {
	        throw new ErrorException("Câu hỏi đã bị xóa trước đó.");
	    }

	    if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
	        throw new ErrorException("Câu hỏi đã được duyệt, không thể xóa.");
	    }

	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

	    DeletionLogEntity deletionLog = DeletionLogEntity.builder()
	            .question(existingQuestion)
	            .reason("Xóa theo yêu cầu của bản thân")
	            .deletedBy(user.getAccount().getUsername())
	            .deletedAt(LocalDateTime.now())
	            .build();

	    deletionLogRepository.save(deletionLog);
	    questionRepository.softDeleteQuestion(questionId);

	    return DataResponse.<Void>builder()
	            .status("success")
	            .message("Câu hỏi đã được xóa thành công.")
	            .build();
	}

	@Override
	public DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content, MultipartFile file, Integer userId) {
	    QuestionEntity parentQuestion = questionRepository.findById(parentQuestionId)
	            .orElseThrow(() -> new ErrorException("Câu hỏi cha không tồn tại"));

	    String fileName = null;
	    if (file != null && !file.isEmpty()) {
	        fileName = saveFile(file);
	    }

	    CreateFollowUpQuestionRequest followUpRequest = CreateFollowUpQuestionRequest.builder()
	            .parentQuestionId(parentQuestionId)
	            .departmentId(parentQuestion.getDepartment().getId())
	            .fieldId(parentQuestion.getField().getId())
	            .roleAskId(parentQuestion.getRoleAsk().getId())
	            .firstName(parentQuestion.getUser().getFirstName())
	            .lastName(parentQuestion.getUser().getLastName())
	            .studentCode(parentQuestion.getUser().getStudentCode())
	            .title(title)
	            .content(content)
	            .statusPublic(parentQuestion.getStatusPublic())
	            .file(file)
	            .statusApproval(false)
	            .build();

	    QuestionDTO followUpQuestionDTO = mapRequestToDTO(followUpRequest, fileName);
	    QuestionEntity followUpQuestion = mapDTOToEntity(followUpQuestionDTO, userId);

	    followUpQuestion.setUser(userRepository.findById(userId)
	            .orElseThrow(() -> new ErrorException("Người dùng không tồn tại")));

	    followUpQuestion.setParentQuestion(parentQuestion);
	    followUpQuestion.setStatusApproval(false);
	    followUpQuestion.setViews(parentQuestion.getViews());

	    QuestionEntity savedFollowUpQuestion = questionRepository.save(followUpQuestion);
	    QuestionDTO savedFollowUpQuestionDTO = mapEntityToDTO(savedFollowUpQuestion);

	    return DataResponse.<QuestionDTO>builder()
	            .status("success")
	            .message("Câu hỏi tiếp theo đã được tạo")
	            .data(savedFollowUpQuestionDTO)
	            .build();
	}
	
	@Override
	@Transactional
	public DataResponse<String> deleteQuestion(Integer questionId, String reason, String username) {
	    QuestionEntity question = questionRepository.findById(questionId)
	            .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

	    Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
	    if (existingLog.isPresent()) {
	        throw new ErrorException("Câu hỏi đã bị xóa trước đó.");
	    }

	    Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(questionId);
	    if (answerOpt.isPresent()) {
	        throw new ErrorException("Không thể xóa câu hỏi vì đã có câu trả lời.");
	    }

	    Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
	    UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

	    if (!"TUVANVIEN".equals(user.getAccount().getRole().getName())) {
	        throw new ErrorException("Bạn không có quyền xóa câu hỏi này.");
	    }

	    DeletionLogEntity deletionLog = DeletionLogEntity.builder()
	            .question(question)
	            .reason(reason)
	            .deletedBy(user.getLastName() + " " + user.getFirstName())
	            .deletedAt(LocalDateTime.now())
	            .build();

	    deletionLogRepository.save(deletionLog);
	    questionRepository.softDeleteQuestion(questionId);

	    return DataResponse.<String>builder()
	            .status("success")
	            .message("Câu hỏi đã được xóa thành công.")
	            .build();
	}

	@Override
	@Transactional
	public DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String username) {
	    UserInformationEntity user = userRepository.findByAccountUsername(username)
	            .orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

	    if (!"TUVANVIEN".equals(user.getAccount().getRole().getName())) {
	        throw new ErrorException("Bạn không có quyền thực hiện chức năng này.");
	    }

	    DepartmentEntity fromDepartment = user.getAccount().getDepartment();
	    if (fromDepartment == null) {
	        throw new ErrorException("Phòng ban của tư vấn viên không tồn tại.");
	    }

	    DepartmentEntity toDepartment = departmentRepository.findById(forwardQuestionRequest.getToDepartmentId())
	            .orElseThrow(() -> new ErrorException("Phòng ban chuyển đến không tồn tại"));

	    if (fromDepartment.getId().equals(toDepartment.getId())) {
	        throw new ErrorException("Không thể chuyển tiếp câu hỏi đến cùng một phòng ban.");
	    }

	    QuestionEntity question = questionRepository.findById(forwardQuestionRequest.getQuestionId())
	            .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

	    UserInformationEntity consultant = userRepository.findById(forwardQuestionRequest.getConsultantId())
	            .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

	    if (!consultant.getAccount().getDepartment().equals(toDepartment)) {
	        throw new ErrorException("Tư vấn viên không thuộc phòng ban chuyển đến.");
	    }

	    ForwardQuestionEntity forwardQuestion = ForwardQuestionEntity.builder()
	            .fromDepartment(fromDepartment)
	            .toDepartment(toDepartment)
	            .question(question)
	            .title("Đã chuyển tiếp câu hỏi từ " + fromDepartment.getName() + " cho " + toDepartment.getName())
	            .statusForward(true)
	            .createdAt(LocalDateTime.now())
	            .build();

	    forwardQuestionRepository.save(forwardQuestion);

	    ForwardQuestionDTO forwardQuestionDTO = mapToForwardQuestionDTO(forwardQuestion);

	    return DataResponse.<ForwardQuestionDTO>builder()
	            .status("success")
	            .message("Câu hỏi đã được chuyển tiếp thành công.")
	            .data(forwardQuestionDTO)
	            .build();
	}
	
	
	
	

	
	
	@Override
    public Page<MyQuestionDTO> getQuestionsWithUserFilters(Integer userId,String title,String status,Integer departmentId,Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(ConsultantSpecification.hasUserQuestion(userId));

        if (title != null && !title.isEmpty()) {
            spec = spec.and(ConsultantSpecification.hasTitle(title));
        }

        if (departmentId != null) {
            spec = spec.and(ConsultantSpecification.hasConsultantsInDepartment(departmentId));
        }

        if (status != null && !status.isEmpty()) {
            QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
            spec = spec.and(ConsultantSpecification.hasStatus(filterStatus));
        }

        Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);

        return questionEntities.map(this::mapToMyQuestionDTO);
    }
	
	public Page<MyQuestionDTO> getQuestionsWithConsultantFilters(Integer consultantId, String title, String status, Pageable pageable) {
	    Specification<QuestionEntity> spec = Specification.where(ConsultantSpecification.hasConsultantAnswer(consultantId));

	    if (title != null && !title.isEmpty()) {
	        spec = spec.and(ConsultantSpecification.hasTitle(title));
	    }

	    if (status != null && !status.isEmpty()) {
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);  
	        spec = spec.and(ConsultantSpecification.hasStatus(filterStatus));
	    }

	    Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);

	    return questionEntities.map(this::mapToMyQuestionDTO);
	}
	
	@Override
	public Page<MyQuestionDTO> getAllQuestionsByDepartmentFilters(Integer departmentId, Pageable pageable) {
	    Page<QuestionEntity> questions = questionRepository.findByDepartmentId(departmentId, pageable);
	    return questions.map(this::mapToMyQuestionDTO);
	}
	
	@Override
    public Page<MyQuestionDTO> getAllQuestionsFilters(Pageable pageable) {
        Page<QuestionEntity> questions = questionRepository.findAll(pageable);
        return questions.map(this::mapToMyQuestionDTO);
    }

	@Override
	public Page<DeletionLogDTO> getDeletedQuestionsByConsultantFilters(String fullName, Pageable pageable) {
	    Specification<DeletionLogEntity> spec = Specification.where(DeletionLogSpecification.hasConsultantFullName(fullName));

	    spec = spec.and(DeletionLogSpecification.hasDeletedStatus());

	    Page<DeletionLogEntity> deletedLogs = deletionLogRepository.findAll(spec, pageable);

	    return deletedLogs.map(this::mapToDeletionLogDTO);
	}
	
	@Override
	public Page<ForwardQuestionDTO> getForwardedQuestionsByDepartmentFilters(String title, Integer toDepartmentId, Pageable pageable) {
	    Specification<ForwardQuestionEntity> spec = Specification.where(ForwardQuestionSpecification.hasToDepartmentId(toDepartmentId));
	    
	    if (title != null && !title.isEmpty()) {
	        spec = spec.and(ForwardQuestionSpecification.hasTitle(title));
	    }
	   
	    Page<ForwardQuestionEntity> forwardedQuestions = forwardQuestionRepository.findAll(spec, pageable);
	    
	    return forwardedQuestions.map(this::mapToForwardQuestionDTO);
	}
	
	@Override
	public Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, Pageable pageable) {
	    Specification<QuestionEntity> spec = Specification.where(ConsultantSpecification.hasConsultantsInDepartment(departmentId));

	    if (title != null && !title.isEmpty()) {
	        spec = spec.and(ConsultantSpecification.hasTitle(title));
	    }

	    if (status != null && !status.isEmpty()) {
	        QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
	        spec = spec.and(ConsultantSpecification.hasStatus(filterStatus));
	    }
	    
	    Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);

	    return questionEntities.map(this::mapToMyQuestionDTO);
	}

	
	
	
	
	

	private QuestionEntity mapDTOToEntity(QuestionDTO questionDTO, Integer userId) {
	    QuestionEntity question = new QuestionEntity();
	    
	    question.setTitle(questionDTO.getTitle());
	    question.setContent(questionDTO.getContent());
	    question.setStatusPublic(questionDTO.getStatusPublic());
	    question.setViews(questionDTO.getViews());

	    UserInformationEntity user = userRepository.findById(userId)
	            .orElseThrow(() -> new RuntimeException("User not found"));
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

	private QuestionDTO mapRequestToDTO(CreateQuestionRequest request, String fileName) {
		return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
				.roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
				.firstName(request.getFirstName()).lastName(request.getLastName()).studentCode(request.getStudentCode())
				.statusPublic(request.getStatusPublic()).fileName(fileName).statusApproval(false).build();
	}
	
	private MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question) {
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
	        
	        .department(departmentDTO)
	        .field(fieldDTO)
	        .roleAsk(roleAskDTO)

	        .build();

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

	private QuestionDTO mapRequestToDTO(CreateFollowUpQuestionRequest request, String fileName) {
		return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
				.roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
				.firstName(request.getFirstName()).lastName(request.getLastName()).studentCode(request.getStudentCode())
				.statusPublic(request.getStatusPublic()).fileName(fileName).build();
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

	    return ForwardQuestionDTO.builder()
	        .title(forwardQuestion.getTitle())
	        .fromDepartment(fromDepartmentDTO)
	        .toDepartment(toDepartmentDTO)
	        .consultant(consultantDTO)
	        .statusForward(forwardQuestion.getStatusForward())
	        .build();
	}
	
	private DeletionLogDTO mapToDeletionLogDTO(DeletionLogEntity deletionLog) {
	    return DeletionLogDTO.builder()
	        .questionId(deletionLog.getQuestion().getId()) 
	        .questionTitle(deletionLog.getQuestion().getTitle()) 
	        .reason(deletionLog.getReason()) 
	        .deletedBy(deletionLog.getDeletedBy()) 
	        .deletedAt(deletionLog.getDeletedAt()) 
	        .build();
	}
} 