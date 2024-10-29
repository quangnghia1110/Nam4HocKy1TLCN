package studentConsulting.service.implement.statistic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.payload.dto.statistic.ConsultantStatisticsDTO;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.content.PostRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.repository.statistic.StatisticsRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.statistic.IStatisticConsultantService;
import studentConsulting.specification.communication.ConversationSpecification;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;
import studentConsulting.specification.content.PostSpecification;
import studentConsulting.specification.question_answer.AnswerSpecification;
import studentConsulting.specification.question_answer.QuestionSpecification;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class StatisticConsultantServiceImpl implements IStatisticConsultantService {

    @Autowired
    private StatisticsRepository statisticsRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RatingRepository ratingRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Override
    public ConsultantStatisticsDTO getConsultantStatistics(Integer consultantId) {
        ConsultantStatisticsDTO statistics = new ConsultantStatisticsDTO();
        LocalDate today = LocalDate.now();
        Integer departmentId = userRepository.findDepartmentIdByConsultantId(consultantId);
        statistics.setTotalQuestionsInDay(statisticsRepository.countQuestionsByDepartmentIdAndDate(departmentId, today));
        statistics.setTotalForwardedQuestions(statisticsRepository.countDistinctToDepartmentsByConsultantIdAndStatusForwardedTrue(consultantId));
        statistics.setTotalDeletedQuestions(statisticsRepository.countByConsultantIdAndDeletedTrue(consultantId));
        statistics.setTotalAnswersGiven(statisticsRepository.countByConsultantIdAndAnsweredTrue(consultantId));
        statistics.setTotalAnswerApproval(statisticsRepository.countByConsultantIdAndStatusApprovalTrue(consultantId));
        statistics.setTotalConsultantSchedule(statisticsRepository.countByConsultantIdAndStatusConfirmedTrue(consultantId));
        statistics.setTotalApprovedPosts(statisticsRepository.countByConsultantIdAndPublishedTrue(consultantId));
        statistics.setTotalConversations(statisticsRepository.countByConsultantId(consultantId));
        return statistics;
    }

    @Override
    public List<Map<String, Object>> getDeletedQuestionsByYear(Integer consultantId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.isDeletedByConsultant())
                .and(QuestionSpecification.hasExactYear(year));

        List<QuestionEntity> questionEntities = questionRepository.findAll(spec);

        for (QuestionEntity question : questionEntities) {
            int month = question.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getAnswersGivenByYear(Integer consultantId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.isAnsweredByConsultant(consultantId))
                .and(AnswerSpecification.hasExactYear(year));

        List<AnswerEntity> answerEntities = answerRepository.findAll(spec);

        for (AnswerEntity answer : answerEntities) {
            int month = answer.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getAnswerApprovalByYear(Integer consultantId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.isPendingApproval(consultantId))
                .and(AnswerSpecification.hasExactYear(year));

        List<AnswerEntity> answerEntities = answerRepository.findAll(spec);

        for (AnswerEntity answer : answerEntities) {
            int month = answer.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getConsultationSchedulesConsultantByYear(Integer consultantId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasConsultant(consultantId))
                .and(ConsultationScheduleSpecification.hasExactYear(year));

        List<ConsultationScheduleEntity> consultationScheduleEntities = consultationScheduleRepository.findAll(spec);

        for (ConsultationScheduleEntity schedule : consultationScheduleEntities) {
            int month = schedule.getConsultationDate().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getApprovedPostsByYear(Integer consultantId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<PostEntity> spec = Specification.where(PostSpecification.isApprovedByConsultant(consultantId))
                .and(PostSpecification.hasExactYear(year));

        List<PostEntity> postEntities = postRepository.findAll(spec);

        for (PostEntity post : postEntities) {
            int month = post.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getConversationsConsultantByYear(Integer consultantId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.hasConsultant(consultantId))
                .and(ConversationSpecification.hasExactYear(year));

        List<ConversationEntity> conversationEntities = conversationRepository.findAll(spec);

        for (ConversationEntity conversation : conversationEntities) {
            int month = conversation.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }
}
