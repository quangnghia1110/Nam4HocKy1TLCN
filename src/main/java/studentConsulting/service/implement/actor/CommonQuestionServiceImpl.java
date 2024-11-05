package studentConsulting.service.implement.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.AnswerEntity;
import studentConsulting.model.entity.CommonQuestionEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.mapper.actor.CommonQuestionMapper;
import studentConsulting.model.payload.request.UpdateCommonQuestionRequest;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.CommonQuestionRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.repository.admin.FieldRepository;
import studentConsulting.repository.admin.RoleAskRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.ICommonQuestionService;
import studentConsulting.specification.actor.CommonQuestionSpecification;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@Service
public class CommonQuestionServiceImpl implements ICommonQuestionService {

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

    @Autowired
    private CommonQuestionMapper commonQuestionMapper;

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        return createCommonQuestionFromQuestionEntity(question, createdByUser);
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestionByDepartment(Integer questionId, Integer departmentId, Principal principal) {
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        if (!question.getDepartment().getId().equals(departmentId)) {
            throw new ErrorException("Câu hỏi không thuộc phòng ban của bạn");
        }

        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        return createCommonQuestionFromQuestionEntity(question, createdByUser);
    }

    private CommonQuestionDTO createCommonQuestionFromQuestionEntity(QuestionEntity question, UserInformationEntity createdByUser) {
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

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            commonQuestion.setAnswerTitle(answer.getTitle());
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(savedCommonQuestion);
    }

    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, UpdateCommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi tổng hợp không tồn tại"));

        return updateCommonQuestionEntity(existingCommonQuestion, request);
    }

    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestionByDepartment(Integer commonQuestionId, Integer departmentId, UpdateCommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi tổng hợp không tồn tại"));

        if (!existingCommonQuestion.getDepartment().getId().equals(departmentId)) {
            throw new ErrorException("Bạn không có quyền chỉnh sửa câu hỏi này.");
        }

        return updateCommonQuestionEntity(existingCommonQuestion, request);
    }

    private CommonQuestionDTO updateCommonQuestionEntity(CommonQuestionEntity commonQuestion, UpdateCommonQuestionRequest request) {
        commonQuestion.setTitle(request.getTitle());
        commonQuestion.setContent(request.getContent());
        if (request.getFileName() != null && !request.getFileName().isEmpty()) {
            String fileName = fileStorageService.saveFile(request.getFileName());
            commonQuestion.setFileName(fileName);
        }
        commonQuestion.setAnswerTitle(request.getAnswerTitle());
        commonQuestion.setAnswerContent(request.getAnswerContent());

        CommonQuestionEntity updatedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(updatedCommonQuestion);
    }

    @Override
    @Transactional
    public void deleteCommonQuestion(Integer id) {
        commonQuestionRepository.deleteById(id);
    }

    @Override
    @Transactional
    public void deleteCommonQuestionByDepartment(Integer id, Integer departmentId) {
        CommonQuestionEntity commonQuestion = commonQuestionRepository.findByIdAndDepartmentId(id, departmentId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi tổng hợp"));
        commonQuestionRepository.delete(commonQuestion);
    }

    @Override
    public CommonQuestionDTO getCommonQuestionById(Integer questionId) {
        return commonQuestionRepository.findById(questionId)
                .map(commonQuestionMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi"));
    }

    @Override
    public CommonQuestionDTO getCommonQuestionByIdAndDepartment(Integer questionId, Integer departmentId) {
        return commonQuestionRepository.findByIdAndDepartmentId(questionId, departmentId)
                .map(commonQuestionMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi"));
    }

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionByRole(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
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
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
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
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }

}
