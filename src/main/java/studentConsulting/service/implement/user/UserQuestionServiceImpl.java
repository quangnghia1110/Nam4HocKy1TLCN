package studentConsulting.service.implement.user;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.RoleAskEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.dto.user.RoleAskDTO;
import studentConsulting.model.payload.request.question_answer.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.model.payload.request.question_answer.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.DeletionLogRepository;
import studentConsulting.repository.question_answer.ForwardQuestionRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.RoleAskRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.interfaces.user.IUserQuestionService;
import studentConsulting.specification.question_answer.QuestionSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class UserQuestionServiceImpl implements IUserQuestionService {

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

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

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
            fileName = fileStorageService.saveFile(questionRequest.getFile());
        }

        QuestionDTO questionDTO = mapRequestToDTO(questionRequest, fileName);
        QuestionEntity question = mapDTOToEntity(questionDTO, userId);
        question.setStatusApproval(false);
        question.setViews(0);

        QuestionEntity savedQuestion = questionRepository.save(question);
        questionRepository.save(savedQuestion);

        QuestionDTO savedQuestionDTO = mapEntityToDTO(savedQuestion);

        return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi đã được tạo")
                .data(savedQuestionDTO).build();
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
        user.setFirstName(request.getFirstName());
        user.setLastName(request.getLastName());
        existingQuestion.setUser(user);

        if (request.getFile() != null && !request.getFile().isEmpty()) {
            String fileName = fileStorageService.saveFile(request.getFile());
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
    public DataResponse<Void> deleteQuestion(Integer questionId, String email) {
        QuestionEntity existingQuestion = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        Optional<DeletionLogEntity> existingLog = deletionLogRepository.findByQuestionId(questionId);
        if (existingLog.isPresent()) {
            throw new ErrorException("Câu hỏi đã bị xóa trước đó.");
        }

        if (Boolean.TRUE.equals(existingQuestion.getStatusApproval())) {
            throw new ErrorException("Câu hỏi đã được duyệt, không thể xóa.");
        }

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        DeletionLogEntity deletionLog = DeletionLogEntity.builder().question(existingQuestion)
                .reason("Xóa theo yêu cầu của bản thân").deletedBy(user.getAccount().getUsername())
                .deletedAt(LocalDate.now()).build();

        deletionLogRepository.save(deletionLog);
        questionRepository.softDeleteQuestion(questionId);

        return DataResponse.<Void>builder().status("success").message("Câu hỏi đã được xóa thành công.").build();
    }

    @Override
    public DataResponse<QuestionDTO> askFollowUpQuestion(Integer parentQuestionId, String title, String content,
                                                         MultipartFile file, Integer userId) {
        QuestionEntity parentQuestion = questionRepository.findById(parentQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi cha không tồn tại"));

        String fileName = null;
        if (file != null && !file.isEmpty()) {
            fileName = fileStorageService.saveFile(file);
        }

        CreateFollowUpQuestionRequest followUpRequest = CreateFollowUpQuestionRequest.builder()
                .parentQuestionId(parentQuestionId).departmentId(parentQuestion.getDepartment().getId())
                .fieldId(parentQuestion.getField().getId()).roleAskId(parentQuestion.getRoleAsk().getId())
                .firstName(parentQuestion.getUser().getFirstName()).lastName(parentQuestion.getUser().getLastName())
                .title(title).content(content).statusPublic(parentQuestion.getStatusPublic()).file(file)
                .statusApproval(false).build();

        QuestionDTO followUpQuestionDTO = mapRequestToDTO(followUpRequest, fileName);
        QuestionEntity followUpQuestion = mapDTOToEntity(followUpQuestionDTO, userId);

        followUpQuestion.setUser(
                userRepository.findById(userId).orElseThrow(() -> new ErrorException("Người dùng không tồn tại")));

        followUpQuestion.setParentQuestion(parentQuestion);
        followUpQuestion.setStatusApproval(false);
        followUpQuestion.setViews(parentQuestion.getViews());

        QuestionEntity savedFollowUpQuestion = questionRepository.save(followUpQuestion);
        QuestionDTO savedFollowUpQuestionDTO = mapEntityToDTO(savedFollowUpQuestion);

        return DataResponse.<QuestionDTO>builder().status("success").message("Câu hỏi tiếp theo đã được tạo")
                .data(savedFollowUpQuestionDTO).build();
    }

    @Override
    public Page<MyQuestionDTO> getAllQuestionsByDepartmentFilters(Integer departmentId, LocalDate startDate,
                                                                  LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification
                .where(QuestionSpecification.hasConsultantsInDepartment(departmentId))
                .and(QuestionSpecification.isPublicAndAnswered());

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questions = questionRepository.findAll(spec, pageable);

        return questions.map(question -> mapToMyQuestionDTO(question, new HashSet<>()));
    }

    @Override
    public Page<MyQuestionDTO> getAllQuestionsFilters(LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification
                .where(QuestionSpecification.isPublicAndAnswered());

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questions = questionRepository.findAll(spec, pageable);

        return questions.map(question -> mapToMyQuestionDTO(question, new HashSet<>()));
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
        question.setCreatedAt(LocalDate.now());

        return question;
    }

    private QuestionDTO mapEntityToDTO(QuestionEntity question) {
        return QuestionDTO.builder()
                .id(question.getId()).departmentId(question.getDepartment().getId()).fieldId(question.getField().getId())
                .roleAskId(question.getRoleAsk().getId()).title(question.getTitle()).content(question.getContent())
                .firstName(question.getUser().getFirstName()).lastName(question.getUser().getLastName())
                .statusPublic(question.getStatusPublic()).fileName(question.getFileName())
                .statusApproval(question.getStatusApproval()).build();
    }

    private QuestionDTO mapRequestToDTO(CreateQuestionRequest request, String fileName) {
        return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
                .roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
                .firstName(request.getFirstName()).lastName(request.getLastName())
                .statusPublic(request.getStatusPublic()).fileName(fileName).statusApproval(false).build();
    }

    private MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question, Set<Integer> processedQuestionIds) {
        // Kiểm tra xem câu hỏi này đã được xử lý chưa
        if (processedQuestionIds.contains(question.getId())) {
            return null; // Hoặc xử lý khác nếu cần
        }

        // Thêm questionId vào tập hợp đã xử lý
        processedQuestionIds.add(question.getId());

        String askerFirstname = question.getUser().getFirstName();
        String askerLastname = question.getUser().getLastName();
        String askerAvatarUrl = question.getUser().getAvatarUrl();

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

        QuestionFilterStatus questionFilterStatus;
        if (Boolean.TRUE.equals(question.getStatusPublic()) && (question.getStatusDelete() == null)) {
            questionFilterStatus = QuestionFilterStatus.PUBLIC;
        } else if (Boolean.TRUE.equals(question.getStatusDelete()) && Boolean.TRUE.equals(question.getStatusPublic())) {
            questionFilterStatus = QuestionFilterStatus.DELETED;
        } else if (Boolean.TRUE.equals(question.getStatusDelete()) && Boolean.FALSE.equals(question.getStatusPublic())) {
            questionFilterStatus = QuestionFilterStatus.DELETED;
        } else if (Boolean.FALSE.equals(question.getStatusPublic()) && (question.getStatusDelete() == null)) {
            questionFilterStatus = QuestionFilterStatus.PRIVATE;
        } else {
            questionFilterStatus = QuestionFilterStatus.NOT_ANSWERED;
        }

        MyQuestionDTO dto = MyQuestionDTO.builder()
                .id(question.getId())
                .title(question.getTitle())
                .content(question.getContent())
                .createdAt(question.getCreatedAt())
                .views(question.getViews())
                .fileName(question.getFileName())
                .askerFirstname(askerFirstname)
                .askerLastname(askerLastname)
                .askerAvatarUrl(askerAvatarUrl)
                .department(departmentDTO)
                .field(fieldDTO)
                .roleAsk(roleAskDTO)
                .filterStatus(questionFilterStatus.getDisplayName())
                .build();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(question.getId());
        answerOpt.ifPresent(answer -> {
            dto.setAnswerTitle(answer.getTitle());
            dto.setAnswerContent(answer.getContent());
            dto.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
            dto.setAnswerUserFirstname(answer.getUser().getFirstName());
            dto.setAnswerUserLastname(answer.getUser().getLastName());
            dto.setAnswerCreatedAt(answer.getCreatedAt());
            dto.setAnswerAvatarUrl(answer.getUser().getAvatarUrl());
        });

        List<MyQuestionDTO> followUpQuestions = getFollowUpQuestions(question.getId(), processedQuestionIds);
        dto.setFollowUpQuestions(followUpQuestions);

        return dto;
    }

    private List<MyQuestionDTO> getFollowUpQuestions(Integer parentQuestionId, Set<Integer> processedQuestionIds) {
        List<QuestionEntity> followUpQuestions = questionRepository.findFollowUpQuestionsByParentId(parentQuestionId);

        return followUpQuestions.stream()
                .map(followUpQuestion -> mapToMyQuestionDTO(followUpQuestion, processedQuestionIds))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }


    private QuestionDTO mapRequestToDTO(CreateFollowUpQuestionRequest request, String fileName) {
        return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
                .roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
                .firstName(request.getFirstName()).lastName(request.getLastName()).studentCode(request.getStudentCode())
                .statusPublic(request.getStatusPublic()).fileName(fileName).build();
    }

}