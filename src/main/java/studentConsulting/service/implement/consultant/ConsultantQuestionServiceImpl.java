package studentConsulting.service.implement.consultant;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
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
import studentConsulting.model.payload.request.question.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.*;
import studentConsulting.repository.common.DepartmentRepository;
import studentConsulting.repository.common.FieldRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.interfaces.consultant.IConsultantQuestionService;
import studentConsulting.specification.DeletionLogSpecification;
import studentConsulting.specification.ForwardQuestionSpecification;
import studentConsulting.specification.QuestionSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.Optional;

@Service
public class ConsultantQuestionServiceImpl implements IConsultantQuestionService {

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
    @Transactional
    public DataResponse<String> deleteQuestion(Integer questionId, String reason, String email) {
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

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        if (!SecurityConstants.Role.TUVANVIEN.equals(user.getAccount().getRole().getName())) {
            throw new ErrorException("Bạn không có quyền xóa câu hỏi này.");
        }

        DeletionLogEntity deletionLog = DeletionLogEntity.builder().question(question).reason(reason)
                .deletedBy(user.getLastName() + " " + user.getFirstName()).deletedAt(LocalDate.now()).build();

        deletionLogRepository.save(deletionLog);
        questionRepository.softDeleteQuestion(questionId);

        return DataResponse.<String>builder().status("success").message("Câu hỏi đã được xóa thành công.").build();
    }

    @Override
    @Transactional
    public DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest,
                                                            String email) {
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        if (!SecurityConstants.Role.TUVANVIEN.equals(user.getAccount().getRole().getName())) {
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

        ForwardQuestionEntity forwardQuestion = ForwardQuestionEntity.builder().fromDepartment(fromDepartment)
                .toDepartment(toDepartment).question(question)
                .title("Đã chuyển tiếp câu hỏi từ " + fromDepartment.getName() + " cho " + toDepartment.getName())
                .statusForward(true).createdAt(LocalDate.now()).build();

        forwardQuestionRepository.save(forwardQuestion);

        ForwardQuestionDTO forwardQuestionDTO = mapToForwardQuestionDTO(forwardQuestion);

        return DataResponse.<ForwardQuestionDTO>builder().status("success")
                .message("Câu hỏi đã được chuyển tiếp thành công.").data(forwardQuestionDTO).build();
    }

    @Override
    public Page<MyQuestionDTO> getQuestionsWithConsultantFilters(Integer consultantId, String title, String status,
                                                                 LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification
                .where(QuestionSpecification.hasConsultantAnswer(consultantId));

        if (title != null && !title.isEmpty()) {
            spec = spec.and(QuestionSpecification.hasTitle(title));
        }

        if (status != null && !status.isEmpty()) {
            QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
            spec = spec.and(QuestionSpecification.hasStatus(filterStatus));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);

        return questionEntities.map(this::mapToMyQuestionDTO);
    }

    @Override
    public Page<DeletionLogDTO> getDeletedQuestionsByConsultantFilters(String fullName, LocalDate startDate,
                                                                       LocalDate endDate, Pageable pageable) {
        Specification<DeletionLogEntity> spec = Specification
                .where(DeletionLogSpecification.hasConsultantFullName(fullName));

        if (startDate != null && endDate != null) {
            spec = spec.and(DeletionLogSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(DeletionLogSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(DeletionLogSpecification.hasDateBefore(endDate));
        }

        spec = spec.and(DeletionLogSpecification.hasDeletedStatus());

        Page<DeletionLogEntity> deletedLogs = deletionLogRepository.findAll(spec, pageable);

        return deletedLogs.map(this::mapToDeletionLogDTO);
    }

    @Override
    public Page<ForwardQuestionDTO> getForwardedQuestionsByDepartmentFilters(String title, Integer toDepartmentId,
                                                                             LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<ForwardQuestionEntity> spec = Specification
                .where(ForwardQuestionSpecification.hasToDepartmentId(toDepartmentId));

        if (title != null && !title.isEmpty()) {
            spec = spec.and(ForwardQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasDateBefore(endDate));
        }

        Page<ForwardQuestionEntity> forwardedQuestions = forwardQuestionRepository.findAll(spec, pageable);

        return forwardedQuestions.map(this::mapToForwardQuestionDTO);
    }

    @Override
    public Page<DeletionLogEntity> getDeletionLogsByConsultant(Integer consultantId, Pageable pageable) {
        UserInformationEntity consultant = userRepository.findById(consultantId)
                .orElseThrow(() -> new ErrorException("Consultant not found"));

        String firstName = consultant.getFirstName();
        String lastName = consultant.getLastName();

        return deletionLogRepository.findAllByConsultantFullName(firstName, lastName, pageable);
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

    private MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question) {
        String askerFirstname = question.getUser().getFirstName();
        String askerLastname = question.getUser().getLastName();
        String askerAvatarUrl = question.getUser().getAvatarUrl(); // Lấy avatar của người hỏi

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
                .questionFilterStatus(questionFilterStatus)
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
                .id(forwardQuestion.getFromDepartment().getId()).name(forwardQuestion.getFromDepartment().getName())
                .build();

        ForwardQuestionDTO.DepartmentDTO toDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getToDepartment().getId()).name(forwardQuestion.getToDepartment().getName())
                .build();

        ForwardQuestionDTO.ConsultantDTO consultantDTO = ForwardQuestionDTO.ConsultantDTO.builder()
                .id(forwardQuestion.getQuestion().getUser().getId())
                .firstName(forwardQuestion.getQuestion().getUser().getFirstName())
                .lastName(forwardQuestion.getQuestion().getUser().getLastName()).build();

        return ForwardQuestionDTO.builder().id(forwardQuestion.getId()).title(forwardQuestion.getTitle()).fromDepartment(fromDepartmentDTO)
                .toDepartment(toDepartmentDTO).consultant(consultantDTO)
                .statusForward(forwardQuestion.getStatusForward()).build();
    }

    private DeletionLogDTO mapToDeletionLogDTO(DeletionLogEntity deletionLog) {
        return DeletionLogDTO.builder().questionId(deletionLog.getQuestion().getId())
                .questionTitle(deletionLog.getQuestion().getTitle()).reason(deletionLog.getReason())
                .deletedBy(deletionLog.getDeletedBy()).deletedAt(deletionLog.getDeletedAt()).build();
    }
}