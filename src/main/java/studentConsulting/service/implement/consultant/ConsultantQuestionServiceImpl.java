package studentConsulting.service.implement.consultant;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.question_answer.ForwardQuestionEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.RoleAskEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.DeletionLogDTO;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.model.payload.request.question_answer.ForwardQuestionRequest;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;
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
import studentConsulting.service.interfaces.consultant.IConsultantQuestionService;
import studentConsulting.specification.question_answer.ForwardQuestionSpecification;
import studentConsulting.specification.question_answer.QuestionSpecification;

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

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Người dùng không tồn tại."));

        if (!SecurityConstants.Role.TUVANVIEN.equals(user.getAccount().getRole().getName())
                && !SecurityConstants.Role.TRUONGBANTUVAN.equals(user.getAccount().getRole().getName())
                && !SecurityConstants.Role.ADMIN.equals(user.getAccount().getRole().getName())) {
            throw new ErrorException("Bạn không có quyền xóa câu hỏi này.");
        }


        DeletionLogEntity deletionLog = DeletionLogEntity.builder().question(question).reason(reason)
                .deletedBy(user.getAccount().getEmail()).deletedAt(LocalDate.now()).build();

        deletionLogRepository.save(deletionLog);
        questionRepository.softDeleteQuestion(questionId);

        return DataResponse.<String>builder().status("success").message("Câu hỏi đã được xóa thành công.").build();
    }

    @Override
    @Transactional
    public DataResponse<ForwardQuestionDTO> forwardQuestion(ForwardQuestionRequest forwardQuestionRequest, String email) {
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

        ForwardQuestionEntity forwardQuestion = ForwardQuestionEntity.builder()
                .fromDepartment(fromDepartment)
                .toDepartment(toDepartment)
                .question(question)
                .title("Đã chuyển tiếp câu hỏi từ " + fromDepartment.getName() + " cho " + toDepartment.getName())
                .statusForward(true)
                .createdAt(LocalDate.now())
                .consultant(consultant)
                .createdBy(user)
                .build();

        forwardQuestionRepository.save(forwardQuestion);

        ForwardQuestionDTO forwardQuestionDTO = mapToForwardQuestionDTO(forwardQuestion, forwardQuestionRequest.getConsultantId());

        return DataResponse.<ForwardQuestionDTO>builder()
                .status("success")
                .message("Câu hỏi đã được chuyển tiếp thành công.")
                .data(forwardQuestionDTO)
                .build();
    }


    @Override
    public Page<MyQuestionDTO> getQuestionAnswerByRole(UserInformationEntity user, String title, String status, Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(null);

        String userRole = user.getAccount().getRole().getName();
        Integer depId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer consultantId = user.getId();
        Integer userId = user.getId();

        if (userRole.equals(SecurityConstants.Role.USER)) {
            spec = spec.and(QuestionSpecification.hasUserQuestion(userId));
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            if (depId != null) {
                spec = spec.and(QuestionSpecification.hasDepartments(depId));
            }
        } else if (userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            if (depId != null) {
                System.out.println(depId);
                spec = spec.and(QuestionSpecification.hasDepartments(depId));
            }

            System.out.println(consultantId);
            spec = spec.and(Specification.where(
                    QuestionSpecification.hasStatus(QuestionFilterStatus.NOT_ANSWERED)
                            .or(QuestionSpecification.hasConsultantAnswer(consultantId))
            ));
        }
        else if(userRole.equals(SecurityConstants.Role.ADMIN)){

        }
        else {
            throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(QuestionSpecification.hasTitle(title));
        }

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasConsultantsInDepartment(departmentId));
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
    public Page<ForwardQuestionDTO> getForwardedQuestionsByDepartmentFilters(String title, Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer consultantId) {

        Specification<ForwardQuestionEntity> spec = Specification
                .where(ForwardQuestionSpecification.hasToDepartmentId(toDepartmentId))
                .and(ForwardQuestionSpecification.hasConsultantAnswer())
                .and(ForwardQuestionSpecification.hasCreatedBy(consultantId));

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

        return forwardedQuestions.map(forwardQuestion -> mapToForwardQuestionDTO(forwardQuestion, forwardQuestion.getConsultant().getId()));
    }


    @Override
    public ForwardQuestionDTO updateForwardQuestion(Integer forwardQuestionId, UpdateForwardQuestionRequest forwardQuestionRequest, Integer consultantId) {
        ForwardQuestionEntity forwardQuestion = forwardQuestionRepository.findById(forwardQuestionId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi chuyển tiếp"));

        if (!forwardQuestion.getConsultant().getId().equals(consultantId)) {
            throw new ErrorException("Bạn không có quyền cập nhật câu hỏi này vì bạn không phải là người đã chuyển tiếp câu hỏi.");
        }

        DepartmentEntity toDepartment = departmentRepository.findById(forwardQuestionRequest.getToDepartmentId())
                .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại"));
        forwardQuestion.setToDepartment(toDepartment);

        QuestionEntity question = questionRepository.findById(forwardQuestionRequest.getQuestionId())
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));
        forwardQuestion.setQuestion(question);

        ForwardQuestionEntity updatedForwardQuestion = forwardQuestionRepository.save(forwardQuestion);

        return mapToForwardQuestionDTO(updatedForwardQuestion, forwardQuestionRequest.getConsultantId());
    }


    @Override
    @Transactional
    public void deleteForwardQuestion(Integer consultantId, Integer forwardQuestionId) {
        ForwardQuestionEntity forwardQuestion = forwardQuestionRepository.findById(forwardQuestionId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi chuyển tiếp"));

        if (!forwardQuestion.getConsultant().getId().equals(consultantId)) {
            throw new ErrorException("Bạn không có quyền xóa câu hỏi này vì bạn không phải là người đã chuyển tiếp câu hỏi.");
        }

        forwardQuestionRepository.delete(forwardQuestion);
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
                .filterStatus(questionFilterStatus.getDisplayName())
                .build();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(question.getId());
        answerOpt.ifPresent(answer -> {
            if (answer.getTitle() == null && answer.getContent() == null) {
                dto.setAnswerTitle(null);
                dto.setAnswerContent(null);
                dto.setAnswerUserEmail(null);
                dto.setAnswerUserFirstname(null);
                dto.setAnswerUserLastname(null);
                dto.setAnswerCreatedAt(null);
                dto.setAnswerAvatarUrl(null);
            } else {
                dto.setAnswerTitle(answer.getTitle());
                dto.setAnswerContent(answer.getContent());
                dto.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
                dto.setAnswerUserFirstname(answer.getUser().getFirstName());
                dto.setAnswerUserLastname(answer.getUser().getLastName());
                dto.setAnswerCreatedAt(answer.getCreatedAt());
                dto.setAnswerAvatarUrl(answer.getUser().getAvatarUrl());
            }
        });


        return dto;
    }

    private ForwardQuestionDTO mapToForwardQuestionDTO(ForwardQuestionEntity forwardQuestion, Integer consultantId) {
        ForwardQuestionDTO.DepartmentDTO fromDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getFromDepartment().getId())
                .name(forwardQuestion.getFromDepartment().getName())
                .build();

        ForwardQuestionDTO.DepartmentDTO toDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getToDepartment().getId())
                .name(forwardQuestion.getToDepartment().getName())
                .build();

        UserInformationEntity consultant = userRepository.findById(consultantId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tư vấn viên"));

        ForwardQuestionDTO.ConsultantDTO consultantDTO = ForwardQuestionDTO.ConsultantDTO.builder()
                .id(consultant.getId())
                .firstName(consultant.getFirstName())
                .lastName(consultant.getLastName())
                .build();

        Integer createdBy = (forwardQuestion.getCreatedBy() != null) ? forwardQuestion.getCreatedBy().getId() : null;

        return ForwardQuestionDTO.builder()
                .id(forwardQuestion.getId())
                .title(forwardQuestion.getTitle())
                .fromDepartment(fromDepartmentDTO)
                .toDepartment(toDepartmentDTO)
                .consultant(consultantDTO)
                .statusForward(forwardQuestion.getStatusForward())
                .createdBy(createdBy)
                .build();
    }
}