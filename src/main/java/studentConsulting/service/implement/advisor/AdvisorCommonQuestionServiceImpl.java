package studentConsulting.service.implement.advisor;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.CommonQuestionEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.CommonQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateCommonQuestionRequest;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.CommonQuestionRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.interfaces.advisor.IAdvisorCommonQuestionService;
import studentConsulting.specification.question_answer.CommonQuestionSpecification;

import javax.transaction.Transactional;
import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdvisorCommonQuestionServiceImpl implements IAdvisorCommonQuestionService {

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.hasDepartment(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsWithAdvisorFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.isCreatedByAdvisor(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(this::mapToDTO);
    }


    private CommonQuestionDTO mapToDTO(CommonQuestionEntity question) {
        return CommonQuestionDTO.builder()
                .commonQuestionId(question.getId())
                .department(CommonQuestionDTO.DepartmentDTO.builder()
                        .id(question.getDepartment().getId())
                        .name(question.getDepartment().getName())
                        .build())
                .field(CommonQuestionDTO.FieldDTO.builder()
                        .id(question.getField().getId())
                        .name(question.getField().getName())
                        .build())
                .roleAsk(CommonQuestionDTO.RoleAskDTO.builder()
                        .id(question.getRoleAsk().getId())
                        .name(question.getRoleAsk().getName())
                        .build())
                .title(question.getTitle())
                .content(question.getContent())
                .fileName(question.getFileName())
                .answerTitle(question.getAnswerTitle())
                .answerContent(question.getAnswerContent())
                .answerUserEmail(question.getAnswerUserEmail())
                .answerUserFirstname(question.getUser().getFirstName())
                .answerUserLastname(question.getUser().getLastName())
                .answerCreatedAt(question.getAnswerCreatedAt())
                .views(question.getViews())
                .createdAt(question.getCreatedAt())
                .askerFirstname(question.getUser().getFirstName())
                .askerLastname(question.getUser().getLastName())
                .createdBy(question.getCreatedBy() != null ? question.getCreatedBy().getLastName() + " " + question.getCreatedBy().getFirstName() : "Unknown")
                .build();
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setFileName(question.getFileName());
        commonQuestion.setCreatedAt(question.getCreatedAt());
        commonQuestion.setViews(question.getViews());
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setField(question.getField());
        commonQuestion.setRoleAsk(question.getRoleAsk());
        commonQuestion.setUser(question.getUser());
        commonQuestion.setCreatedBy(createdByUser);

        if (question.getUser() != null) {
            commonQuestion.setAskerFirstname(question.getUser().getFirstName());
            commonQuestion.setAskerLastname(question.getUser().getLastName());
        } else {
            commonQuestion.setAskerFirstname("Unknown");
            commonQuestion.setAskerLastname("Unknown");
        }

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());

            if (answer.getUser() != null && answer.getUser().getAccount() != null) {
                commonQuestion.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
                commonQuestion.setAnswerUserFirstname(answer.getUser().getFirstName());
                commonQuestion.setAnswerUserLastname(answer.getUser().getLastName());
            } else {
                commonQuestion.setAnswerUserEmail("unknown@example.com");
                commonQuestion.setAnswerUserFirstname("Unknown");
                commonQuestion.setAnswerUserLastname("Unknown");
            }
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
            commonQuestion.setAnswerTitle(answer.getTitle());
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return mapToDTO(savedCommonQuestion);
    }


    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, Integer departmentId, UpdateCommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new RuntimeException("Câu hỏi tổng hợp không tồn tại"));

        if (!existingCommonQuestion.getDepartment().getId().equals(departmentId)) {
            throw new RuntimeException("Không có quyền cập nhật câu hỏi này vì nó không thuộc phòng ban của bạn.");
        }

        existingCommonQuestion.setTitle(request.getTitle());
        existingCommonQuestion.setContent(request.getContent());
        if (request.getFileName() != null && !request.getFileName().isEmpty()) {
            String fileName = fileStorageService.saveFile(request.getFileName());
            existingCommonQuestion.setFileName(fileName);
        }


        existingCommonQuestion.setAnswerTitle(request.getAnswerTitle());
        existingCommonQuestion.setAnswerContent(request.getAnswerContent());

        CommonQuestionEntity updatedCommonQuestion = commonQuestionRepository.save(existingCommonQuestion);

        return mapToDTO(updatedCommonQuestion);
    }

    @Override
    @Transactional
    public void deleteCommonQuestion(Integer id, Integer departmentId) {
        Optional<CommonQuestionEntity> commonQuestionOpt = commonQuestionRepository.findByIdAndDepartmentId(id, departmentId);
        if (!commonQuestionOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy câu hỏi tổng hợp.");
        }

        commonQuestionRepository.delete(commonQuestionOpt.get());
    }
}