package studentConsulting.service.implement.statistic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.payload.dto.statistic.UserStatisticsDTO;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.content.PostRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.repository.statistic.StatisticsRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.statistic.IStatisticUserService;
import studentConsulting.specification.communication.ConversationSpecification;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;
import studentConsulting.specification.question_answer.QuestionSpecification;
import studentConsulting.specification.rating.RatingSpecification;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class StatisticUserServiceImpl implements IStatisticUserService {

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
    public UserStatisticsDTO getUserStatistics(Integer userId) {
        UserStatisticsDTO statistics = new UserStatisticsDTO();
        statistics.setTotalQuestions(statisticsRepository.countTotalQuestionsByUser(userId));
        statistics.setQuestionsOver500Views(statisticsRepository.countQuestionsWithMoreThan500Views(userId));
        statistics.setTotalAppointments(statisticsRepository.countConsultationSchedulesByUser(userId));
        statistics.setTotalRatings(statisticsRepository.countRatingsByUser(userId));
        return statistics;
    }

    @Override
    public List<Map<String, Object>> getStatisticsByYear(Integer userId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.hasUserQuestion(userId))
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
    public List<Map<String, Object>> getRatingsByYear(Integer userId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasUser(userId))
                .and(RatingSpecification.hasExactYear(year));

        List<RatingEntity> ratingEntities = ratingRepository.findAll(spec);

        for (RatingEntity rating : ratingEntities) {
            int month = rating.getSubmittedAt().getMonthValue();
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
    public List<Map<String, Object>> getConsultationSchedulesByYear(Integer userId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        System.out.println("UserId: " + userId);
        System.out.println("Year: " + year);

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasUser(userId))
                .and(ConsultationScheduleSpecification.hasExactYear(year));

        List<ConsultationScheduleEntity> appointmentEntities = consultationScheduleRepository.findAll(spec);
        System.out.println("Số lượng lịch hẹn tìm thấy: " + appointmentEntities.size());

        for (ConsultationScheduleEntity schedule : appointmentEntities) {
            int month = schedule.getCreatedAt().getMonthValue();
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
    public List<Map<String, Object>> getConversationsByYear(Integer userId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.isOwner(userId))
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

    @Override
    public List<Map<String, Object>> getConversationsMemberByYear(Integer userId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.isMember(userId))
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